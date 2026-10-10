#' The session to ride for a comparison
#'
#' The block-by-block protocol from `vignette("recording_rides")`, as data. Each
#' row is one interval: how long, whether the trainer should hold the target,
#' and what the target is as a fraction of FTP.
#'
#' Each block exists for something the pipeline needs, and `why` carries the
#' reason:
#'
#' * The **sync surges** are not there to find the offset — when the app records,
#'   one clock stamps every sample and the offset is zero by construction. They
#'   are there because [align_rides()] cannot tell "these clocks agree" from
#'   "these traces cannot be compared at all" and collapses both into
#'   `unresolved`. Without an edge to lock onto, a recorder that stamped every
#'   meter wrongly passes exactly as a correct one does.
#' * The **ladder** puts a chosen number of seconds into each band, because
#'   [agreement_by_level()] only has a band if you spent time in it.
#' * The ladder **runs up and then back down** because a meter reading high
#'   through warming up gives different numbers at the same target on the way
#'   down, where a level-dependent meter repeats itself. Ridden one way only,
#'   [level_dependence()] cannot separate them.
#'
#' `mode` is `"erg"` where the trainer holds the target and `"free"` where it
#' must not: a trainer in erg fights a surge and rounds off the very edge the
#' sync block exists to provide.
#'
#' There is one sync block, at the front, and no second one at the end. A
#' closing block would only detect a clock drifting during the ride, and when
#' the app records there is one clock stamping every meter — it cannot drift
#' relative to itself, so the block would cost the best part of two minutes to
#' rule out something that cannot happen. Independent head units are a different
#' case, where two clocks can separate; that route is prose in
#' `vignette("recording_rides")` rather than this function, and it should repeat
#' the surges at the end.
#'
#' @param step_seconds Seconds per ladder step. The binding constraint is not
#'   `min_seconds` but how many bootstrap blocks each band carries: the up and
#'   down ladders land in the same bands, so a band holds roughly
#'   `2 * step_seconds`, and the default 120 gives it about four 60-second
#'   blocks. Going shorter drops under three, and resampling two blocks is not
#'   an estimate. Lengthening to 240 buys about 17% narrower bands for twice
#'   the ladder.
#' @param surges Number of coast-to-power edges in the sync block.
#'
#' @return A tibble of `block`, `seconds`, `mode`, `target_pc`, `cue` and
#'   `why`, one row per interval, in riding order. `cue` is what the rider is
#'   told at the time; `why` is what the block is for.
#'
#' @examples
#' comparison_protocol()
#' sum(comparison_protocol()$seconds) / 60
#' @export
comparison_protocol <- function(step_seconds = 120, surges = 4) {
  ladder <- c(0.55, 0.70, 0.85, 1.00, 1.10)

  # A surge then its recovery, repeated. The block before this one already ends
  # in a coast, so starting on the surge gives `surges` coast-to-power edges
  # rather than one fewer.
  sync <- function(label, why) {
    tibble::tibble(
      block = label,
      seconds = rep(c(8, 20), times = surges),
      mode = "free",
      target_pc = rep(c(1.5, 0), times = surges),
      cue = rep(
        c("Out of the saddle, hard", "Stop pedalling completely"),
        times = surges
      ),
      why = why
    )
  }

  step <- function(label, targets, cue, why) {
    tibble::tibble(
      block = label,
      seconds = step_seconds,
      mode = "erg",
      target_pc = targets,
      cue = cue,
      why = why
    )
  }

  dplyr::bind_rows(
    tibble::tibble(
      block = "Zero and settle",
      seconds = 30,
      mode = "free",
      target_pc = 0,
      cue = "Zero-offset every meter, then sit still",
      why = "A shared floor of true zeros at the head of every file"
    ),
    sync(
      "Sync surges",
      "Coast-to-power edges, so check_alignment() can resolve rather than default"
    ),
    step(
      "Ladder up",
      ladder,
      "Hold the target and a steady cadence",
      "Several hundred stable seconds in each band"
    ),
    tibble::tibble(
      block = "Coast",
      seconds = 60,
      mode = "free",
      target_pc = 0,
      cue = "Stop pedalling; marks the halves for the drift check",
      why = "Exercises the coasting drop, and marks the halves for the drift check"
    ),
    step(
      "Ladder down",
      rev(ladder),
      "The same targets, later in the ride",
      "The same bands, later in the ride"
    ),
    tibble::tibble(
      block = "Stop together",
      seconds = 30,
      mode = "free",
      target_pc = 0,
      cue = "Sit still, then stop every meter at once",
      why = "A shared tail"
    )
  )
}

duration_label <- function(seconds) {
  each <- function(x) {
    if (x %% 60 == 0) paste0(x / 60, " min") else paste0(x, " s")
  }
  distinct <- unique(seconds)
  repeats <- length(seconds) / length(distinct)
  parts <- paste(vapply(distinct, each, character(1)), collapse = " / ")
  if (repeats == 1) {
    parts
  } else if (length(distinct) == 1) {
    paste0(repeats, " \u00d7 ", parts)
  } else {
    paste0(repeats, " \u00d7 (", parts, ")")
  }
}

# A block that is entirely coasting has nothing for the trainer to hold, so
# naming a mode there invites the rider to set one.
block_mode <- function(mode, target_pc) {
  if (all(target_pc == 0)) {
    "\u2014"
  } else if (all(mode == "erg")) {
    "Erg"
  } else {
    "Level or slope"
  }
}

target_label <- function(mode, target_pc, ftp) {
  watts <- round(ftp * target_pc)
  if (all(target_pc == 0)) {
    "Coasting"
  } else if (all(mode == "erg")) {
    paste(paste0(watts, " W"), collapse = " \u2192 ")
  } else {
    paste0("About ", max(watts), " W, coasting between")
  }
}

#' The protocol as blocks to read
#'
#' [comparison_protocol()] is one row per interval, which a workout file needs
#' and a rider does not. This collapses consecutive rows back into the blocks
#' they came from, so the table on the Record tab and the one in
#' `vignette("recording_rides")` are both the protocol rather than a
#' transcription of it that can drift from the workout file.
#'
#' @param protocol One row per interval, as [comparison_protocol()] returns.
#' @param ftp Functional threshold power in watts, which turns the fractional
#'   targets into the watts the trainer will be asked to hold. The default is
#'   the package's reference rider; set your own.
#'
#' @return A data frame of `Block`, `Duration`, `Mode`, `Target` and `Why`, one
#'   row per block, with a closing `Total`.
#'
#' @examples
#' collapse_protocol(comparison_protocol(), ftp = 250)
#' @export
collapse_protocol <- function(protocol, ftp = 250) {
  runs <- rle(protocol$block)
  blocks <- split(protocol, rep(seq_along(runs$lengths), runs$lengths))

  out <- do.call(
    rbind,
    lapply(blocks, function(rows) {
      data.frame(
        Block = rows$block[1],
        Duration = duration_label(rows$seconds),
        Mode = block_mode(rows$mode, rows$target_pc),
        Target = target_label(rows$mode, rows$target_pc, ftp),
        Why = rows$why[1]
      )
    })
  )

  rbind(
    out,
    data.frame(
      Block = "Total",
      Duration = paste0(round(sum(protocol$seconds) / 60), " min"),
      Mode = "",
      Target = "",
      Why = "Ride it identically every time, so a chained comparison can rest on the bridge meter reading the same in both runs"
    )
  )
}

#' Write the protocol as a trainer workout file
#'
#' Turns [comparison_protocol()] into something a training app can ride, so the
#' session is commanded rather than remembered. The format follows the file
#' extension, the way [read_file()] dispatches on it.
#'
#' `.zwo` (Zwift) carries the distinction the protocol depends on: erg steps are
#' written as `SteadyState` and the sync surges and coasts as `FreeRide`, which
#' releases erg control for exactly the blocks that must not be ridden in it.
#' `.erg` (Golden Cheetah, TrainerDay, PerfPro) has no such concept and is a
#' plain watts-against-time course, so the surge blocks come out as targets the
#' trainer will try to hold. Drop out of erg for them by hand.
#'
#' The trainer holding the targets must be commanded over ANT+: while the Record
#' tab has the trainer's Bluetooth connection, nothing else can have it.
#'
#' @param path File to write. The extension chooses the format: `.zwo` or
#'   `.erg`.
#' @param ftp Functional threshold power in watts. `.zwo` stores targets as
#'   fractions of the rider's FTP, so this only reaches the description there;
#'   `.erg` stores absolute watts, where a wrong FTP is a wrong workout. The
#'   default is the package's reference rider; set your own.
#' @param protocol A protocol from [comparison_protocol()].
#'
#' @return `path`, invisibly.
#'
#' @examples
#' path <- tempfile(fileext = ".zwo")
#' write_workout(path, ftp = 250)
#' @export
write_workout <- function(path, ftp = 250, protocol = comparison_protocol()) {
  format <- tolower(tools::file_ext(path))
  switch(
    format,
    zwo = write_zwo(path, ftp, protocol),
    erg = write_erg(path, ftp, protocol),
    stop(
      "No workout format for '",
      format,
      "'. Write a .zwo for Zwift or a .erg for Golden Cheetah and TrainerDay."
    )
  )
  invisible(path)
}

#' @noRd
workout_description <- function(ftp) {
  paste0(
    "Power meter comparison, built by cyclingpower at ",
    round(ftp),
    " W FTP. The surge and coast blocks must be ridden as free ride: a trainer ",
    "in erg rounds off the coast-to-power edges that let check_alignment() ",
    "resolve rather than default. The ladder runs up and back down so a ",
    "meter that drifts as it warms can be told from one whose error depends ",
    "on the power."
  )
}

#' @noRd
write_zwo <- function(path, ftp, protocol) {
  doc <- xml2::xml_new_root("workout_file")
  xml2::xml_add_child(doc, "author", "cyclingpower")
  xml2::xml_add_child(doc, "name", "Power meter comparison")
  xml2::xml_add_child(doc, "description", workout_description(ftp))
  xml2::xml_add_child(doc, "sportType", "bike")
  workout <- xml2::xml_add_child(doc, "workout")

  for (i in seq_len(nrow(protocol))) {
    row <- protocol[i, ]
    interval <- if (row$mode == "erg") {
      xml2::xml_add_child(
        workout,
        "SteadyState",
        Duration = as.character(row$seconds),
        Power = format(row$target_pc, nsmall = 2)
      )
    } else {
      xml2::xml_add_child(
        workout,
        "FreeRide",
        Duration = as.character(row$seconds),
        FlatRoad = "1"
      )
    }

    # A free-ride block commands nothing, so the instruction is the only thing
    # carrying it. Watts rather than per cent: it is being read mid-surge.
    message <- if (row$mode == "erg") {
      paste0(row$block, ": ", row$cue)
    } else {
      paste0(
        row$block,
        ": ",
        row$cue,
        if (row$target_pc > 0) {
          paste0(" (about ", round(ftp * row$target_pc), " W)")
        } else {
          ""
        }
      )
    }
    xml2::xml_add_child(
      interval,
      "textevent",
      timeoffset = "0",
      message = message
    )
  }

  xml2::write_xml(doc, path)
}

#' @noRd
write_erg <- function(path, ftp, protocol) {
  # Two rows per block at the same watts, so the course is a square wave rather
  # than a ramp between targets: a ramp would fill every band with transient
  # seconds recorded while both meters were still responding to a change, and
  # meters respond at different speeds.
  ends <- cumsum(protocol$seconds)
  starts <- ends - protocol$seconds
  watts <- round(ftp * protocol$target_pc)

  minutes <- as.vector(rbind(starts, ends)) / 60
  power <- as.vector(rbind(watts, watts))

  writeLines(
    c(
      "[COURSE HEADER]",
      "VERSION = 2",
      "UNITS = ENGLISH",
      paste0("DESCRIPTION = ", workout_description(ftp)),
      paste0("FILE NAME = ", basename(path)),
      paste0("FTP = ", round(ftp)),
      "MINUTES\tWATTS",
      "[END COURSE HEADER]",
      "[COURSE DATA]",
      sprintf("%.4f\t%d", minutes, power),
      "[END COURSE DATA]"
    ),
    path
  )
}
