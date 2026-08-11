#' Summarise a ride with the Coggan metrics
#'
#' Replaces `skimr`'s default numeric summary with the power metrics, so a
#' grouped tibble yields one row of metrics per group. Useful for a per-source
#' overview alongside the paired comparison, though note the zero-handling
#' caveat in [drop_zeros()].
#'
#' @param power_data Tibble with a `power` column, optionally grouped.
#' @param ftp Functional threshold power in watts.
#'
#' @return A `skimr` summary with duration, average power, NP, TSS, IF and VI.
#'
#' @examples
#' \dontrun{
#' read_file("ride.fit") |> dplyr::select(power) |> skim_power_data(ftp = 275)
#' }
#' @export
skim_power_data <- function(power_data, ftp = 275) {
  power_skimr <- skimr::skim_with(
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
