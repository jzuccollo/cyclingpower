#' skim_power_data
#'
#' @param power_data tbl
#'
#' @return
skim_power_data <- function(power_data) {
  power_only_data <-
    power_data %>%
    dplyr::select(tidyselect::contains("power"))

  power_skimr <-
    skimr::skim_with(
      numeric = skimr::sfl(
        Duration_minutes = ~ length(.) / 60,
        avg_power = average_power,
        NP = normalised_power,
        TSS = ~ tss(., 275),
        IF = ~ intensity_factor(., 275),
        VI = variability_index
      ),
      append = FALSE
    )
  power_skimr(power_only_data)
}
