#' Put meters from different rides on one scale
#'
#' Three meters cannot always be ridden together. The usual shape is that run
#' one carries A and B, run two carries B and C, and you want A against C
#' even though they never turned a pedal at the same time. B is the bridge:
#' each run gives a gain relative to it, and the difference between those two
#' gains is A against C.
#'
#' The gains are estimated by taking the log ratio second by second within a
#' run, which cancels the true power without having to model it, and the
#' interval comes from resampling the two runs independently.
#'
#' This rests on B reading the same on both days, which the data cannot check:
#' any drift in B between rides transfers directly onto the A-to-C comparison
#' and nothing in the ride files distinguishes that from a real difference.
#' What can be checked is reported by [chain_report()] — whether the two runs
#' covered comparable power, since gains compared over different parts of the
#' range are not really comparable at all.
#'
#' @param aligned Result of [align_rides()].
#' @param bridge Source common to more than one run. Found automatically when
#'   exactly one source qualifies.
#' @inheritParams power_agreement
#'
#' @return A tibble with one row per source pair, giving the gain of the first
#'   relative to the second as a percentage with its interval, and a `basis`
#'   column recording whether the pair was observed together (`direct`) or
#'   linked through the bridge (`chained`). Diagnostics are attached; see
#'   [chain_report()].
#'
#' @examples
#' \dontrun{
#' read_rides(spec) |> align_rides() |> chain_sources()
#' }
#' @export
chain_sources <- function(
  aligned,
  bridge = NULL,
  min_power = 0,
  block = 60,
  replicates = 1000
) {
  bridge <- resolve_bridge(aligned, bridge)
  pairs <- bridge_pairs(aligned, bridge, min_power)

  if (nrow(pairs) == 0) {
    stop(
      "No seconds where ",
      bridge,
      " and another meter both read above ",
      min_power,
      " W, so there is nothing to chain from. Lower min_power."
    )
  }

  groups <- split(pairs, list(pairs$run, pairs$source), drop = TRUE)

  # Each run is resampled on its own, which is what makes the interval on a
  # chained comparison wider than either of the gains it is built from.
  estimates <- lapply(groups, function(g) {
    list(
      run = g$run[[1]],
      source = g$source[[1]],
      seconds = nrow(g),
      point = mean(g$log_gain),
      draws = block_bootstrap_draws(
        nrow(g),
        function(idx) mean(g$log_gain[idx]),
        block,
        replicates
      )
    )
  })

  pooled <- pool_by_source(estimates)
  pooled[[bridge]] <- list(
    source = bridge,
    seconds = NA_integer_,
    point = 0,
    draws = rep(0, replicates)
  )

  out <- compare_all_sources(pooled, aligned, bridge)
  attr(out, "chain") <- chain_diagnostics(pairs, bridge)
  out
}

#' @rdname chain_sources
#' @param chained Result of [chain_sources()].
#' @export
chain_report <- function(chained) {
  attr(chained, "chain")
}

#' Sources that could bridge two runs
#'
#' A meter can only link runs it appeared in more than one of. This is what
#' [chain_sources()] searches when `bridge` is left unset, exposed separately so
#' anything offering the choice to a user works from the same list.
#'
#' @param aligned Result of [align_rides()].
#'
#' @return A character vector of source names, empty when nothing qualifies.
#'
#' @examples
#' \dontrun{
#' read_rides(spec) |> align_rides() |> bridge_candidates()
#' }
#' @export
bridge_candidates <- function(aligned) {
  presence <- unique(aligned[, c("run", "source")])
  runs_per_source <- table(presence$source)
  names(runs_per_source)[runs_per_source > 1]
}

#' @noRd
resolve_bridge <- function(aligned, bridge) {
  candidates <- bridge_candidates(aligned)

  if (!is.null(bridge)) {
    if (!bridge %in% candidates) {
      stop(
        bridge,
        " appears in only one run, so it cannot link the others. Sources in ",
        "more than one run: ",
        if (length(candidates)) paste(candidates, collapse = ", ") else "none"
      )
    }
    return(bridge)
  }

  if (length(candidates) == 0) {
    stop(
      "No source appears in more than one run, so there is nothing to chain ",
      "through. Comparing meters across runs needs one meter present in both."
    )
  }
  if (length(candidates) > 1) {
    stop(
      "More than one source could be the bridge (",
      paste(candidates, collapse = ", "),
      "). Name the one to use with bridge = ."
    )
  }

  candidates
}

#' Log gain of each source against the bridge. `paired_seconds()` orders pairs
#' alphabetically, so the ratio is flipped when the bridge sorts first.
#' @noRd
bridge_pairs <- function(aligned, bridge, min_power) {
  paired_seconds(aligned, min_power) |>
    dplyr::filter(.data$source_a == bridge | .data$source_b == bridge) |>
    dplyr::mutate(
      source = ifelse(.data$source_a == bridge, .data$source_b, .data$source_a),
      log_gain = ifelse(
        .data$source_a == bridge,
        -.data$log_ratio,
        .data$log_ratio
      )
    )
}

#' A meter can appear against the bridge in several runs, which happens as soon
#' as you ride the same pair twice. Combine those into one gain weighted by the
#' seconds behind each, keeping the bootstrap draws on the same weights so the
#' interval narrows as the evidence accumulates.
#' @noRd
pool_by_source <- function(estimates) {
  by_source <- split(
    estimates,
    vapply(estimates, `[[`, character(1), "source")
  )

  lapply(by_source, function(group) {
    seconds <- vapply(group, `[[`, numeric(1), "seconds")
    weight <- seconds / sum(seconds)

    list(
      source = group[[1]]$source,
      seconds = sum(seconds),
      point = sum(weight * vapply(group, `[[`, numeric(1), "point")),
      draws = Reduce(`+`, Map(function(g, w) w * g$draws, group, weight))
    )
  })
}

#' @noRd
compare_all_sources <- function(named, aligned, bridge) {
  sources <- sort(names(named))
  shared <- shared_runs(aligned)

  utils::combn(sources, 2, simplify = FALSE) |>
    lapply(function(pair) {
      first <- named[[pair[[1]]]]
      second <- named[[pair[[2]]]]
      draws <- first$draws - second$draws
      ci <- percentile_ci(draws)

      tibble::tibble(
        source_a = pair[[1]],
        source_b = pair[[2]],
        basis = if (shared[[paste(pair, collapse = "|")]]) {
          "direct"
        } else {
          "chained"
        },
        gain_pc = 100 * (exp(first$point - second$point) - 1),
        gain_lo = 100 * (exp(ci[[1]]) - 1),
        gain_hi = 100 * (exp(ci[[2]]) - 1),
        via = if (shared[[paste(pair, collapse = "|")]]) {
          NA_character_
        } else {
          bridge
        }
      )
    }) |>
    dplyr::bind_rows()
}

#' @noRd
shared_runs <- function(aligned) {
  presence <- unique(aligned[, c("run", "source")])
  by_run <- split(presence$source, presence$run)
  sources <- sort(unique(presence$source))

  keys <- utils::combn(sources, 2, simplify = FALSE)
  stats::setNames(
    vapply(
      keys,
      function(pair) {
        any(vapply(by_run, function(s) all(pair %in% s), logical(1)))
      },
      logical(1)
    ),
    vapply(keys, paste, character(1), collapse = "|")
  )
}

#' The chain assumes the bridge behaved the same on both days. That cannot be
#' tested here, but comparing the power each run actually covered shows
#' whether the two gains describe the same sort of riding.
#' @noRd
chain_diagnostics <- function(pairs, bridge) {
  coverage <- pairs |>
    dplyr::group_by(.data$run) |>
    dplyr::summarise(
      seconds = dplyr::n(),
      median_power = stats::median(.data$level),
      p10 = stats::quantile(.data$level, 0.1),
      p90 = stats::quantile(.data$level, 0.9),
      .groups = "drop"
    )

  overlap <- max(0, min(coverage$p90) - max(coverage$p10)) /
    (max(coverage$p90) - min(coverage$p10))

  list(
    bridge = bridge,
    coverage = coverage,
    overlap = overlap,
    assumption = paste0(
      "Chained comparisons assume ",
      bridge,
      " read the same in every run. Nothing in the ride files can test that; ",
      "any drift in ",
      bridge,
      " between runs is indistinguishable from a real difference between the ",
      "meters being compared."
    )
  )
}

#' How the chained comparison varies with power
#'
#' The level-wise counterpart to [chain_sources()]. Each run's gain against the
#' bridge is estimated within power bands, and the bands kept are those both
#' runs actually covered: a gain chained from bands one run never rode is
#' extrapolation, not measurement.
#'
#' @inheritParams chain_sources
#' @inheritParams agreement_by_level
#'
#' @return A tibble with one row per band per source pair, restricted to bands
#'   covered by every run involved.
#'
#' @examples
#' \dontrun{
#' read_rides(spec) |> align_rides() |> chain_by_level()
#' }
#' @export
chain_by_level <- function(
  aligned,
  bridge = NULL,
  width = 25,
  min_seconds = 30,
  min_power = 0
) {
  bridge <- resolve_bridge(aligned, bridge)

  banded <- bridge_pairs(aligned, bridge, min_power) |>
    dplyr::mutate(band = floor(.data$level / width) * width + width / 2) |>
    dplyr::group_by(.data$run, .data$source, .data$band) |>
    dplyr::summarise(
      seconds = dplyr::n(),
      log_gain = mean(.data$log_gain),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$seconds >= min_seconds)

  common <- Reduce(intersect, split(banded$band, banded$run))
  banded <- dplyr::filter(banded, .data$band %in% common)

  sources <- sort(unique(banded$source))
  pairs <- utils::combn(c(sources, bridge) |> sort(), 2, simplify = FALSE)

  lapply(pairs, function(pair) {
    a <- band_gains(banded, pair[[1]], bridge, common)
    b <- band_gains(banded, pair[[2]], bridge, common)

    tibble::tibble(
      source_a = pair[[1]],
      source_b = pair[[2]],
      band = common,
      gain_pc = 100 * (exp(a - b) - 1)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(.data$gain_pc))
}

#' @noRd
band_gains <- function(banded, source, bridge, bands) {
  if (source == bridge) {
    return(rep(0, length(bands)))
  }
  rows <- banded[banded$source == source, ]
  rows$log_gain[match(bands, rows$band)]
}
