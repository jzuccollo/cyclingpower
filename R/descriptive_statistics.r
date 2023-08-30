#' skim_power_data
#'
#' @param power_data tbl
#'
#' @return
skim_power_data <- function(power_data, ftp = 275) {
  message("FTP: ", ftp)
  power_skimr <-
    skimr::skim_with(
      numeric = skimr::sfl(
        duration_minutes = ~ length(.) / 60,
        avg_power = average_power,
        NP = normalised_power,
        TSS = ~ tss(., ftp),
        IF = ~ intensity_factor(., ftp),
        VI = variability_index
      ),
      append = FALSE
    )
  power_skimr(power_data)
}
