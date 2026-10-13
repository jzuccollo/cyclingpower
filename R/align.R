#' Put sources onto a common one-second clock
#'
#' Two meters recording the same ride do not necessarily agree about what time
#' it is. When each records to its own head unit the clocks can differ by
#' several seconds, and differencing the series on nominal timestamps then
#' compares power from one moment against power from another. That inflates
#' the spread, shifts the bias, and looks entirely plausible on the way out.
#'
#' Within each run every source is resampled onto a one-second grid, then the
#' offset against a reference source is estimated by maximising the
#' cross-correlation of the power series. Where both channels came from a
#' single head unit the estimate is zero, so the same path serves both cases.
#' Gaps left by pauses or smart recording stay `NA` rather than being
#' interpolated across.
#'
#' @param rides Long tibble from [read_rides()].
#' @param max_offset Largest offset to consider, in seconds. Set to `0` to
#'   trust the recorded timestamps and skip estimation.
#'
#' @return A tibble of `run`, `source`, `t` (seconds from the start of the
#'   run), `time` and the ride channels, with an alignment report attached.
#'   Retrieve the report with [alignment_report()].
#'
#' @family alignment
#' @examples
#' aligned <- read_rides(demo_rides()) |> align_rides()
#'
#' # The demo plants a seven-second offset on one meter, and it comes back
#' alignment_report(aligned)
#' @export
align_rides <- function(rides, max_offset = 30) {
  runs <- split(rides, rides$run)
  aligned <- lapply(runs, align_one_run, max_offset)

  out <- dplyr::bind_rows(lapply(aligned, `[[`, "data"))
  attr(out, "alignment") <- dplyr::bind_rows(lapply(aligned, `[[`, "report"))
  out
}

#' @rdname align_rides
#' @param aligned Result of [align_rides()].
#' @export
alignment_report <- function(aligned) {
  attr(aligned, "alignment")
}

#' @noRd
align_one_run <- function(ride, max_offset) {
  origin <- min(ride$time)
  gridded <- lapply(split(ride, ride$source), resample_1hz, origin = origin)
  span <- max(vapply(gridded, function(x) max(x$t), numeric(1)))
  gridded <- lapply(gridded, pad_to_grid, span = span)

  # The source with the most complete power trace makes the steadiest anchor.
  coverage <- vapply(gridded, function(x) sum(!is.na(x$power)), numeric(1))
  reference <- names(gridded)[which.max(coverage)]

  report <- lapply(names(gridded), function(source) {
    if (source == reference || max_offset == 0) {
      fit <- list(lag = 0, correlation = NA_real_, sharpness = NA_real_)
    } else {
      fit <- best_lag(
        gridded[[reference]]$power,
        gridded[[source]]$power,
        max_offset
      )
    }
    tibble::tibble(
      run = ride$run[[1]],
      source = source,
      reference = reference,
      offset = fit$lag,
      correlation = fit$correlation,
      sharpness = fit$sharpness
    )
  }) |>
    dplyr::bind_rows()

  report <- check_alignment(report)

  shifted <- Map(
    function(x, offset) dplyr::mutate(x, t = .data$t - offset),
    gridded,
    report$offset[match(names(gridded), report$source)]
  )

  data <- dplyr::bind_rows(shifted, .id = "source") |>
    dplyr::mutate(run = ride$run[[1]], .before = 1) |>
    dplyr::mutate(time = origin + .data$t)

  list(data = data, report = report)
}

#' Collapse to whole seconds, averaging any duplicate or sub-second samples.
#' Without this the later join fans out rows instead of pairing them.
#' @noRd
resample_1hz <- function(x, origin) {
  x |>
    dplyr::mutate(t = round(as.numeric(.data$time - origin, units = "secs"))) |>
    dplyr::select(-dplyr::any_of(c("time", "run", "source"))) |>
    dplyr::group_by(.data$t) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ replace(.x, is.nan(.x), NA)
    )) |>
    fill_jitter()
}

#' A meter notifying about once a second, stamped on arrival, lands some
#' seconds twice and others not at all: rounding onto the grid left a Kickr
#' with 14% of its seconds empty, which blanked its rolling mean almost
#' everywhere. Gaps of up to two seconds are interpolated, which covers that
#' jitter. Anything longer is a dropout and stays missing, so a lost connection
#' is still not counted as agreement.
#' @noRd
fill_jitter <- function(x) {
  tibble::tibble(t = seq(min(x$t), max(x$t))) |>
    dplyr::left_join(x, by = "t") |>
    dplyr::mutate(dplyr::across(
      -"t",
      ~ zoo::na.approx(.x, maxgap = 2, na.rm = FALSE)
    ))
}

#' @noRd
pad_to_grid <- function(x, span) {
  dplyr::left_join(tibble::tibble(t = seq(0, span)), x, by = "t")
}

#' Element `i` of the result is `y[i + k]`, so a positive `k` looks ahead.
#' @noRd
shift_by <- function(y, k) {
  idx <- seq_along(y) + k
  idx[idx < 1 | idx > length(y)] <- NA_integer_
  y[idx]
}

#' A positive lag means the candidate's own timestamps run that many seconds
#' ahead of the reference, so correcting it means subtracting the lag.
#' @noRd
best_lag <- function(reference, candidate, max_offset) {
  lags <- seq(-max_offset, max_offset)

  cors <- vapply(
    lags,
    function(k) {
      shifted <- shift_by(candidate, k)
      ok <- !is.na(reference) & !is.na(shifted)
      if (
        sum(ok) < 30 ||
          stats::sd(reference[ok]) == 0 ||
          stats::sd(shifted[ok]) == 0
      ) {
        return(NA_real_)
      }
      stats::cor(reference[ok], shifted[ok])
    },
    numeric(1)
  )

  if (all(is.na(cors))) {
    return(list(lag = 0, correlation = NA_real_, sharpness = NA_real_))
  }

  best <- which.max(cors)
  far <- abs(lags - lags[best]) >= 5 & !is.na(cors)

  list(
    lag = lags[best],
    correlation = cors[best],
    sharpness = if (any(far)) cors[best] - max(cors[far]) else NA_real_
  )
}

#' How well the traces must match at the best offset, and how far that peak
#' must stand above the rest of the surface, for the offset to count as
#' established. Both are judgement calls, not tuning parameters: below these,
#' the data did not settle the question.
#' @noRd
MIN_CORRELATION <- 0.5
MIN_SHARPNESS <- 0.02

#' Two situations produce the same flat correlation surface: files that are
#' not the same ride, and a steady effort with no features to lock onto. They
#' cannot be told apart from the correlation alone, and refusing to proceed
#' would block the steady trainer sessions this package is mostly used on. So
#' both resolve to one state: the offset was not established, the recorded
#' timestamps stand, and the report carries the numbers that say why.
#' @noRd
check_alignment <- function(report) {
  estimated <- !is.na(report$correlation)

  resolved <- estimated &
    report$correlation >= MIN_CORRELATION &
    !is.na(report$sharpness) &
    report$sharpness >= MIN_SHARPNESS

  report$status <- dplyr::case_when(
    !estimated ~ "assumed",
    resolved ~ "aligned",
    .default = "unresolved"
  )
  report$offset[report$status == "unresolved"] <- 0

  unresolved <- report[report$status == "unresolved", ]
  if (nrow(unresolved) > 0) {
    warning(
      "Could not establish a clock offset for ",
      paste(unresolved$source, collapse = ", "),
      " against ",
      report$reference[[1]],
      " (best correlation ",
      paste(round(unresolved$correlation, 2), collapse = ", "),
      ", peak sharpness ",
      paste(round(unresolved$sharpness, 3), collapse = ", "),
      "). Falling back to the recorded timestamps. This is expected for a ",
      "steady effort with no features to lock onto; a low correlation instead ",
      "suggests these are not recordings of the same ride."
    )
  }

  report
}
