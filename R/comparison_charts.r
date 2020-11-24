#' compare_ts
#'
#' @param rundata tbl
#'
#' @return
compare_ts <- function(rundata) {
  rundata %>%
    tidylog::pivot_longer(
      -timestamp,
      names_to = "source",
      values_to = "power"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$timestamp, y = .data$power, colour = .data$source)) +
    tidyquant::geom_ma(n = 30, linetype = "solid", size = 1) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::labs(
      x = "Time",
      y = "Power (watts)",
      title = "Comparison of power data across sources",
      subtitle = "1-second power and 30-second moving average"
    ) +
    ggplot2::scale_color_brewer(type = "qual")
}

#' compare_distribution
#'
#' @param rundata tbl
#' @param pm1 var First power variable
#' @param pm2 var Second power variable
#'
#' @return
compare_distribution <- function(rundata, pm1, pm2) {
  rundata %>%
    tidylog::drop_na() %>%
    tidylog::mutate(
      power_diff = {{ pm1 }} - {{ pm2 }},
      median_power_diff_pc = 100 * stats::median(power_diff) / stats::median({{ pm2 }})
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = power_diff)) +
    ggplot2::geom_histogram(binwidth = 2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stats::median(power_diff)), linetype = "dashed") +
    ggplot2::geom_text(
      ggplot2::aes(
        x = median(power_diff),
        y = -2,
        label = glue::glue(
          "Median difference: {stats::median(power_diff)} watts \\
      ({scales::comma(first(median_power_diff_pc))}%)"
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
    )
}
