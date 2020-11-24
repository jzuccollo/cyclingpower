#' extract_data
#'
#' @param file_data tbl
#' @param file_ext chr
#'
#' @return
extract_data <- function(file_data, file_ext) {
  if (file_ext == "fit") {
    return(
      FITfileR::records(file_data) %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(timestamp)
    )
  } else {
    return(file_data)
  }
}

#' read_file
#'
#' @param file_url chr
#'
#' @return
#' @export
read_file <- function(file_url) {
  file_ext <- tolower(tools::file_ext(file_url))
  if (file_ext == "fit") {
    read_fun <- FITfileR::readFitFile
  } else if (file_ext == "gpx") {
    read_fun <- trackeR::readGPX
  } else {
    stop(glue::glue("{file_url} is not a FIT or GPX file."))
  }
  extract_data(read_fun(file_url), file_ext)
}