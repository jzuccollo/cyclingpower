#' normalised_power
#'
#' @param power_data dbl
#'
#' @return
#' @export
normalised_power <- function(power_data) {
  mean(zoo::rollmean(power_data, 30)^4, na.rm = TRUE)^0.25
}

#' average_power
#'
#' @param power_data dbl
#'
#' @return
#' @export
average_power <- function(power_data) {
  mean(power_data, na.rm = TRUE)
}

#' intensity_factor
#'
#' @param power_data dbl
#' @param ftp dbl FTP
#'
#' @return
#' @export
intensity_factor <- function(power_data, ftp) {
  normalised_power(power_data) / ftp
}

#' tss
#'
#' @param power_data dbl
#' @param ftp dbl FTP
#'
#' @return
#' @export
tss <- function(power_data, ftp) {
  t <- length(power_data)
  intensity_factor <- intensity_factor(power_data, ftp)
  (t * intensity_factor^2) / 36
}

#' variability_index
#'
#' @param power_data dbl
#'
#' @return
#' @export
variability_index <- function(power_data) {
  normalised_power(power_data) / average_power(power_data)
}
