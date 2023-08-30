#' compare_ts
#'
#' @param rundata tbl
#' @param runvar chr variable denoting the different runs
#'
#' @return
compare_ts <- function(rundata, runvar) {
  rundata |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$timestamp,
      y = .data$power,
      colour = .data$power_source
    )) +
    tidyquant::geom_ma(n = 30, linetype = "solid", size = 1) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::labs(
      x = "Time",
      y = "Power (watts)",
      title = "Comparison of power data across sources",
      subtitle = "1-second power and 30-second moving average"
    ) +
    ggplot2::scale_color_brewer(type = "qual") +
    ggplot2::facet_wrap(runvar, ncol = 1, scales = "free_x")
}

#' compare_distribution
#'
#' @param rundata tbl
#' @param runvar chr variable denoting the different runs
#'
#' @return
compare_distribution <- function(rundata, runvar) {
  power_sources <- unique(rundata$power_source)
  if (length(power_sources) > 2) {
    stop("Too many power sources to difference.")
  }
  pm1 <- rlang::sym(power_sources[[1]])
  pm2 <- rlang::sym(power_sources[[2]])

  reshaped_power <- rundata |>
    tidyr::pivot_wider(names_from = "power_source", values_from = "power") |>
    tidylog::drop_na() |>
    tidylog::mutate(power_diff = {{ pm1 }} - {{ pm2 }})

  median_power <- reshaped_power |>
    dplyr::group_by(!!rlang::sym(runvar)) |>
    dplyr::summarise(
      median_power_diff = stats::median(.data$power_diff),
      median_power_diff_pc = 100 * .data$median_power_diff / stats::median({{ pm2 }})
    )

  reshaped_power |>
    ggplot2::ggplot(ggplot2::aes(x = .data$power_diff)) +
    ggplot2::geom_histogram(binwidth = 2) +
    ggplot2::geom_vline(
      data = .data$median_power,
      ggplot2::aes(xintercept = .data$median_power_diff),
      linetype = "dashed"
    ) +
    ggplot2::geom_text(
      data = .data$median_power,
      ggplot2::aes(
        x = .data$median_power_diff,
        y = -2,
        label = glue::glue(
          "Median difference: {median_power_diff} watts ({scales::comma(median_power_diff_pc)}%)"
        )
      ),
      hjust = 0,
      nudge_x = 2
    ) +
    ggplot2::scale_colour_brewer(type = "qual") +
    ggplot2::labs(
      x = glue::glue("Difference in power (power meter - Kickr Core)"),
      y = "Frequency",
      title = "Distribution of difference in power",
      subtitle = "1-second differences in power, where power is recorded on both devices"
    ) +
    ggplot2::facet_wrap(runvar, ncol = 1, scales = "free_y")
}


#' diff_by_var
#'
#' @param rundata tbl
#' @param pm1 var First power variable
#' @param pm2 var Second power variable
#' @param by var x var to plot diff against
#'
#' @return
diff_by_var <- function(rundata, pm1, pm2, by) {
  rundata |>
    tidylog::drop_na() |>
    tidylog::mutate(power_diff = {{ pm1 }} - {{ pm2 }}) |>
    ggplot2::ggplot(ggplot2::aes(x = {{ by }}, y = power_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth() +
    ggplot2::scale_colour_brewer() +
    ggplot2::labs(
      y = glue::glue("Difference in power (power meter - Kickr Core)"),
      x = "by_var",
      title = "Difference in power by by_var",
      subtitle = "1-second differences in power, where power is recorded on both devices"
    )
}
