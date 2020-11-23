#' normalised_power
#'
#' @param ride_data tbl
#'
#' @return
#' @export
normalised_power <- function(ride_data) {
  mean(zoo::rollmean(ride_data$power, 30) ^ 4) ^ 0.25
}

#' average_power
#'
#' @param ride_data tbl
#'
#' @return
#' @export
average_power <- function(ride_data) {
  mean(ride_data$power)
}

#' intensity_factor
#'
#' @param ride_data tbl
#' @param ftp dbl FTP
#'
#' @return
#' @export
intensity_factor <- function(ride_data, ftp) {
  normalised_power(ride_data) / ftp
}

#' tss
#'
#' @param ride_data tbl
#' @param ftp dbl FTP
#'
#' @return
#' @export
tss <- function(ride_data, ftp) {
  t <- length(ride_data$power)
  intensity_factor <- intensity_factor(ride_data, ftp)
  (t * intensity_factor^2) / 36
}

#' variability_index
#'
#' @param ride_data tbl
#'
#' @return
#' @export
variability_index <- function(ride_data) {
  normalised_power(ride_data) / average_power(ride_data)
}
