#' Okabe-Ito, which stays distinguishable under every common form of colour
#' blindness and in greyscale.
#' @noRd
source_palette <- function() {
  c(
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442",
    "#000000"
  )
}

#' Facet strips are sized up (ggplot renders them at about 6 px, and the strip
#' is what names the pair a panel shows) and titles down, so a plot inside a
#' card does not shout over the card's own heading.
#'
#' No colour scale here: only the plots mapping a source or a basis want the
#' Okabe-Ito series, and adding it would silently replace the Bland-Altman
#' plot's own scale.
#' @noRd
chart_theme <- function() {
  list(
    ggplot2::theme_minimal(base_size = 12),
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, colour = "grey35"),
      plot.caption = ggplot2::element_text(size = 9, colour = "grey45"),
      # Tinted rather than bare: stacked panels put the strip immediately
      # under the previous panel's tick labels, where an unbacked label reads
      # as an axis title for the panel above rather than a header for the one
      # below.
      strip.text = ggplot2::element_text(
        size = 11,
        face = "bold",
        margin = ggplot2::margin(4, 6, 4, 6)
      ),
      strip.background = ggplot2::element_rect(fill = "#f0efed", colour = NA),
      panel.spacing = ggplot2::unit(1.1, "lines"),
      panel.grid.minor = ggplot2::element_blank()
    )
  )
}

#' One panel per pair actually ridden together. Faceting on run and comparison
#' as a grid draws the full cross product, and a pair generally appears in one
#' run only, so most of that grid is empty cells. Wrapping on a single label
#' draws the combinations that exist. The run is left out of the label when
#' there is only one, where it says nothing.
#' @noRd
panel_label <- function(x) {
  comparison <- paste(x$source_a, "-", x$source_b)

  x$panel <- if (dplyr::n_distinct(x$run) > 1) {
    paste0(comparison, "  (", x$run, ")")
  } else {
    comparison
  }
  x
}

#' Power over time, by source
#'
#' The first look at any comparison: whether the traces sit on top of each
#' other, and whether they are in step. A rolling mean is drawn over the
#' second-by-second trace, which is too noisy to read directly.
#'
#' @param aligned Result of [align_rides()].
#' @param smooth Window of the rolling mean, in seconds.
#'
#' @return A ggplot object.
#'
#' @family plots
#' @examples
#' read_rides(demo_rides()) |> align_rides() |> plot_timeseries()
#' @export
plot_timeseries <- function(aligned, smooth = 30) {
  smoothed <- aligned |>
    dplyr::group_by(.data$run, .data$source) |>
    dplyr::mutate(
      rolling = zoo::rollmean(.data$power, smooth, fill = NA, align = "center")
    ) |>
    dplyr::ungroup()

  # The raw layer is drawn faint enough that two sources overlaid do not bury
  # each other, and the rolling means — the lines actually being compared —
  # are drawn heavier and, being a later layer, entirely on top of them.
  ggplot2::ggplot(smoothed, ggplot2::aes(x = .data$t, colour = .data$source)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$power), alpha = 0.12) +
    ggplot2::geom_line(ggplot2::aes(y = .data$rolling), linewidth = 1) +
    ggplot2::scale_colour_manual(values = source_palette()) +
    ggplot2::facet_wrap("run", ncol = 1, scales = "free_x") +
    ggplot2::labs(
      x = "Seconds from the start of the run",
      y = "Power (watts)",
      colour = "Source",
      title = "Power recorded by each source",
      subtitle = paste0(
        "Faint line is second-by-second power; solid line is a ",
        smooth,
        "-second rolling mean"
      ),
      caption = "Sources shown on a common clock after alignment."
    ) +
    chart_theme()
}

#' Bland-Altman plot of the difference between two meters
#'
#' Each second contributes a point: how far apart the two meters read, against
#' the power they were reading. Plotting against the mean of the pair rather
#' than against either one avoids the artefact where a meter's own noise pulls
#' the cloud into a slope that looks like a real trend.
#'
#' The dashed lines are the limits of agreement, between which most seconds
#' fall. A cloud that tilts is a meter whose error changes with power.
#'
#' Seconds are binned into a two-dimensional histogram rather than drawn as
#' points. An hour carries 3,600 of them, and at that density overlapping
#' translucent points are a smear whose darkest region is wherever the ride
#' happened to spend its time — which is the one thing the plot is not asking
#' about. The count scale is logarithmic, so a band holding ten seconds is
#' still visible beside one holding three hundred.
#'
#' The panel is drawn at a fixed aspect ratio. A Bland-Altman plot's job is to
#' let you see a tilt, and letting the height float free of the width flattens
#' the apparent slope by an amount that depends on the reader's window.
#'
#' @inheritParams power_agreement
#'
#' @return A ggplot object.
#'
#' @family plots
#' @examples
#' read_rides(demo_rides()) |> align_rides() |> plot_agreement()
#' @export
plot_agreement <- function(aligned, min_power = 0) {
  pairs <- paired_seconds(aligned, min_power) |>
    panel_label()

  # One row per line rather than three geom_hline layers, so the mean, the
  # limits and the zero line all get a legend entry that names them.
  lines <- pairs |>
    dplyr::group_by(.data$panel) |>
    dplyr::summarise(
      bias = mean(.data$difference),
      spread = stats::sd(.data$difference),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Zero = 0,
      `Mean difference` = .data$bias,
      `Lower limit of agreement` = .data$bias - 1.96 * .data$spread,
      `Upper limit of agreement` = .data$bias + 1.96 * .data$spread
    ) |>
    tidyr::pivot_longer(
      c(
        "Zero",
        "Mean difference",
        "Lower limit of agreement",
        "Upper limit of agreement"
      ),
      names_to = "line",
      values_to = "y"
    ) |>
    dplyr::mutate(
      key = factor(
        ifelse(grepl("limit", .data$line), "Limits of agreement", .data$line),
        levels = c("Zero", "Mean difference", "Limits of agreement")
      )
    )

  ggplot2::ggplot(pairs, ggplot2::aes(x = .data$level, y = .data$difference)) +
    ggplot2::geom_bin2d(bins = 60) +
    ggplot2::scale_fill_gradient(
      low = "#dbe7f0",
      high = source_palette()[[1]],
      transform = "log10",
      name = "Seconds"
    ) +
    ggplot2::geom_hline(
      data = lines,
      ggplot2::aes(
        yintercept = .data$y,
        colour = .data$key,
        linetype = .data$key
      ),
      linewidth = 0.6
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        Zero = "grey40",
        `Mean difference` = source_palette()[[2]],
        `Limits of agreement` = source_palette()[[2]]
      ),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        Zero = "solid",
        `Mean difference` = "solid",
        `Limits of agreement` = "dashed"
      ),
      name = NULL
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel)) +
    ggplot2::labs(
      x = "Power, averaged across the two meters (watts)",
      y = "Difference (watts)",
      title = "How far apart the meters read, and where",
      subtitle = "Each cell counts the seconds spent there, on a log scale",
      caption = "Coasting seconds are excluded. A tilted cloud means the difference depends on power."
    ) +
    chart_theme() +
    ggplot2::theme(aspect.ratio = 0.62)
}

#' Difference between two meters across the power range
#'
#' The summary a single average difference hides. Each band shows the mean
#' difference over the seconds spent at that power, with its confidence
#' interval; the bars beneath show how many seconds sit behind each estimate,
#' because time is not spread evenly across the range and the ends are usually
#' thin.
#'
#' On the per-cent scale the caption says why the two scales differ: a meter
#' that looks level-dependent in watts is often only a constant percentage
#' out.
#'
#' @param by_level Result of [agreement_by_level()].
#' @param scale `"watts"` for the difference in watts, `"percent"` for the
#'   gain. A constant percentage error is a rising line in watts and a flat
#'   line in per cent, so the two say different things about the same meter.
#'
#' @return A ggplot object.
#'
#' @family plots
#' @examples
#' read_rides(demo_rides()) |>
#'   align_rides() |>
#'   agreement_by_level() |>
#'   plot_by_level()
#' @export
plot_by_level <- function(by_level, scale = c("watts", "percent")) {
  scale <- match.arg(scale)
  plotted <- panel_label(by_level)

  cols <- if (scale == "watts") {
    c("bias_w", "bias_lo", "bias_hi")
  } else {
    c("gain_pc", "gain_lo", "gain_hi")
  }

  ggplot2::ggplot(
    plotted,
    ggplot2::aes(x = .data$mean_power, y = .data[[cols[[1]]]])
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[cols[[2]]]], ymax = .data[[cols[[3]]]]),
      alpha = 0.2,
      fill = source_palette()[[1]]
    ) +
    ggplot2::geom_line(colour = source_palette()[[1]], linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$seconds),
      colour = source_palette()[[1]]
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel)) +
    ggplot2::scale_size_continuous(range = c(0.5, 3)) +
    ggplot2::labs(
      x = "Power, averaged across the two meters (watts)",
      y = if (scale == "watts") {
        "Difference (watts)"
      } else {
        "Difference (%)"
      },
      size = "Seconds",
      title = if (scale == "watts") {
        "Difference between the meters, by how hard you were riding"
      } else {
        "Gain between the meters, by how hard you were riding"
      },
      subtitle = "Shaded band is the 95% interval from a moving-block bootstrap",
      caption = paste(
        "Point size shows the seconds behind each estimate; sparse bands are the least reliable.",
        if (scale == "percent") {
          "\nA constant percentage error is a rising line in watts and a flat line in per cent."
        }
      )
    ) +
    chart_theme()
}

#' Gains between meters on one scale
#'
#' Shows every pairwise comparison as a percentage, separating the pairs that
#' were ridden together from those linked through a bridge meter. The chained
#' comparisons carry the uncertainty of both rides and rest on the bridge
#' having read the same on both days, so they are drawn distinctly rather than
#' presented as equivalent evidence.
#'
#' @param chained Result of [chain_sources()].
#'
#' @return A ggplot object.
#'
#' @family plots
#' @examples
#' read_rides(demo_rides()) |> align_rides() |> chain_sources() |> plot_chain()
#' @export
plot_chain <- function(chained) {
  plotted <- chained |>
    dplyr::mutate(
      comparison = paste(.data$source_a, "vs", .data$source_b),
      basis = factor(.data$basis, levels = c("direct", "chained"))
    )

  ggplot2::ggplot(
    plotted,
    ggplot2::aes(x = .data$gain_pc, y = .data$comparison, colour = .data$basis)
  ) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$gain_lo, xmax = .data$gain_hi),
      orientation = "y",
      width = 0.15,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$basis), size = 3) +
    ggplot2::scale_colour_manual(values = source_palette()) +
    ggplot2::scale_shape_manual(
      values = c(direct = 16, chained = 17),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Gain of the first meter relative to the second (%)",
      y = NULL,
      colour = "Basis",
      shape = "Basis",
      title = "How the meters compare",
      subtitle = "Positive means the first meter reads higher",
      caption = paste(
        "Chained pairs were never ridden together and are linked through a",
        "bridge meter, assumed to read the same on both days."
      )
    ) +
    chart_theme()
}
