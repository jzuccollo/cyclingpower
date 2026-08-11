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
  seconds <- paired_seconds(aligned, min_power)
  pairs <- bridge_pairs(seconds, bridge)

  if (nrow(pairs) == 0) {
    stop(
      "No seconds where ",
      bridge,
      " and another meter both read above ",
      min_power,
      " W, so there is nothing to chain from. Lower min_power."
    )
  }

  # Sources the bridge never met cannot be placed on its scale, so resampling
  # the pairs they belong to would only spend the time.
  linked <- c(unique(pairs$source), bridge)
  observed <- pair_estimates(
    dplyr::filter(
      seconds,
      .data$source_a %in% linked,
      .data$source_b %in% linked
    ),
    block,
    replicates
  )
  gains <- bridge_gains(observed, unique(pairs$source), bridge, replicates)

  out <- compare_all_sources(gains, observed, bridge)
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

#' A pair is keyed on its two sources in the order `paired_seconds()` puts them,
#' which is alphabetical, so a key built from either end finds the same rows.
#' @noRd
pair_key <- function(sources) {
  paste(sort(sources), collapse = "|")
}

#' The seconds a pair rode together, restricted to the pairs involving the
#' bridge and oriented as log(source / bridge). Only the coverage diagnostics
#' need this now; the gains themselves come from `pair_estimates()`.
#' @noRd
bridge_pairs <- function(seconds, bridge) {
  seconds |>
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

#' Every pair that was actually ridden together, as a log gain of the first
#' source over the second with its bootstrap draws, pooled over the runs
#' carrying the pair.
#'
#' Each run is resampled on its own, which is what makes the interval on a
#' chained comparison wider than either of the gains it is built from.
#' @noRd
pair_estimates <- function(seconds, block, replicates) {
  key <- paste(seconds$source_a, seconds$source_b, sep = "|")

  split(seconds, list(key, seconds$run), drop = TRUE) |>
    lapply(function(g) {
      list(
        pair = paste(g$source_a[[1]], g$source_b[[1]], sep = "|"),
        seconds = nrow(g),
        point = mean(g$log_ratio),
        draws = block_bootstrap_draws(
          nrow(g),
          function(idx) mean(g$log_ratio[idx]),
          block,
          replicates
        )
      )
    }) |>
    pool_runs()
}

#' A pair can appear in several runs, which happens as soon as you ride the same
#' pair twice. Combine those into one gain weighted by the seconds behind each,
#' keeping the bootstrap draws on the same weights so the interval narrows as
#' the evidence accumulates.
#' @noRd
pool_runs <- function(estimates) {
  by_pair <- split(estimates, vapply(estimates, `[[`, character(1), "pair"))

  lapply(by_pair, function(group) {
    seconds <- vapply(group, `[[`, numeric(1), "seconds")
    weight <- seconds / sum(seconds)

    list(
      pair = group[[1]]$pair,
      seconds = sum(seconds),
      point = sum(weight * vapply(group, `[[`, numeric(1), "point")),
      draws = Reduce(`+`, Map(function(g, w) w * g$draws, group, weight))
    )
  })
}

#' Each source on the bridge's scale, which is what makes two meters from
#' different runs comparable. The gain is the pair's own estimate, negated where
#' the bridge sorts first, since a pair is stored as log(first / second).
#' @noRd
bridge_gains <- function(observed, sources, bridge, replicates) {
  gains <- lapply(sources, function(source) {
    estimate <- observed[[pair_key(c(source, bridge))]]
    direction <- if (bridge < source) -1 else 1

    list(
      point = direction * estimate$point,
      draws = direction * estimate$draws
    )
  })
  names(gains) <- sources

  gains[[bridge]] <- list(point = 0, draws = rep(0, replicates))
  gains
}

#' @noRd
compare_all_sources <- function(gains, observed, bridge) {
  sources <- sort(names(gains))

  utils::combn(sources, 2, simplify = FALSE) |>
    lapply(function(pair) {
      # A pair that rode together is reported from its own seconds. Routing it
      # through the bridge anyway would answer from two independent resamples a
      # question the pair's own data settles, and call the result direct.
      estimate <- observed[[pair_key(pair)]]
      chained <- is.null(estimate)

      if (chained) {
        estimate <- list(
          point = gains[[pair[[1]]]]$point - gains[[pair[[2]]]]$point,
          draws = gains[[pair[[1]]]]$draws - gains[[pair[[2]]]]$draws
        )
      }
      ci <- percentile_ci(estimate$draws)

      tibble::tibble(
        source_a = pair[[1]],
        source_b = pair[[2]],
        basis = if (chained) "chained" else "direct",
        gain_pc = 100 * (exp(estimate$point) - 1),
        gain_lo = 100 * (exp(ci[[1]]) - 1),
        gain_hi = 100 * (exp(ci[[2]]) - 1),
        via = if (chained) bridge else NA_character_
      )
    }) |>
    dplyr::bind_rows()
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
#' The level-wise counterpart to [chain_sources()], and on the same rule: a pair
#' that rode together is compared within each power band from its own seconds,
#' and only a pair that never did is chained through the bridge. The bands kept
#' are those every run actually covered, since a gain chained from bands one run
#' never rode is extrapolation rather than measurement.
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

  banded <- paired_seconds(aligned, min_power) |>
    dplyr::mutate(band = floor(.data$level / width) * width + width / 2) |>
    dplyr::group_by(
      .data$run,
      .data$source_a,
      .data$source_b,
      .data$band
    ) |>
    dplyr::summarise(
      seconds = dplyr::n(),
      log_ratio = mean(.data$log_ratio),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$seconds >= min_seconds)

  common <- Reduce(intersect, split(banded$band, banded$run))

  # A pair can appear in more than one run, and then has a row per run in every
  # band. Pooled on seconds, the same weighting chain_sources() uses, so the two
  # do not report different numbers for the same pair.
  pooled <- banded |>
    dplyr::filter(.data$band %in% common) |>
    dplyr::group_by(.data$source_a, .data$source_b, .data$band) |>
    dplyr::summarise(
      log_ratio = stats::weighted.mean(.data$log_ratio, .data$seconds),
      .groups = "drop"
    )

  sources <- bridged_sources(pooled, bridge)
  pairs <- utils::combn(sources, 2, simplify = FALSE)

  lapply(pairs, function(pair) {
    tibble::tibble(
      source_a = pair[[1]],
      source_b = pair[[2]],
      band = common,
      gain_pc = 100 * (exp(band_pair_gain(pooled, pair, bridge, common)) - 1)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(.data$gain_pc))
}

#' The sources a chained comparison can reach: the bridge, and everything that
#' met it in a band both runs covered.
#' @noRd
bridged_sources <- function(pooled, bridge) {
  met <- pooled[pooled$source_a == bridge | pooled$source_b == bridge, ]
  sort(unique(c(
    bridge,
    ifelse(met$source_a == bridge, met$source_b, met$source_a)
  )))
}

#' Band by band, on the same rule as `compare_all_sources()`: a pair that rode
#' together speaks for itself, and only a pair that never did goes through the
#' bridge.
#' @noRd
band_pair_gain <- function(pooled, pair, bridge, bands) {
  direct <- band_ratios(pooled, pair[[1]], pair[[2]], bands)
  if (!all(is.na(direct))) {
    return(direct)
  }
  band_gains(pooled, pair[[1]], bridge, bands) -
    band_gains(pooled, pair[[2]], bridge, bands)
}

#' @noRd
band_gains <- function(pooled, source, bridge, bands) {
  if (source == bridge) {
    return(rep(0, length(bands)))
  }
  key <- sort(c(source, bridge))
  ratio <- band_ratios(pooled, key[[1]], key[[2]], bands)
  if (key[[1]] == bridge) -ratio else ratio
}

#' @noRd
band_ratios <- function(pooled, source_a, source_b, bands) {
  rows <- pooled[
    pooled$source_a == source_a & pooled$source_b == source_b,
  ]
  rows$log_ratio[match(bands, rows$band)]
}
