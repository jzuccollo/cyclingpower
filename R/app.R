#' Explore a comparison interactively
#'
#' Opens a local web interface over the same pipeline as [compare_report()]:
#' point it at a directory of ride files, correct the run and source labels if
#' the filenames do not carry them, and read the results with the settings live.
#'
#' The report is still the thing to keep — it is a file you can send or come
#' back to — and the app's download button renders it from the results already
#' on screen, so the two cannot disagree.
#'
#' @param dir Directory of ride files to open with. Can also be set in the app.
#' @param ... Passed to [shiny::runApp()], for `port` and `host`.
#'
#' @return Nothing useful; called for the running app.
#'
#' @examples
#' \dontrun{
#' launch_app("~/rides")
#' }
#' @export
launch_app <- function(dir = NULL, ...) {
  needed <- c("shiny", "bslib", "DT", "shinyFiles")
  missing <- needed[
    !vapply(needed, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "The app needs ",
      paste(missing, collapse = ", "),
      ". Install with install.packages(c(\"",
      paste(missing, collapse = "\", \""),
      "\"))."
    )
  }

  # The app is built from functions in this package rather than run from a
  # directory, so nothing serves inst/shiny/www automatically any more. Without
  # this the two scripts 404, the Record tab still renders, and the Pair button
  # does nothing with no error anywhere to find.
  shiny::addResourcePath(
    www_prefix,
    system.file("shiny", "www", package = "cyclingpower")
  )

  # runApp() takes no arguments of its own, so the starting directory travels
  # as a shiny option rather than through the call.
  shiny::shinyOptions(cyclingpower_dir = dir)
  shiny::runApp(shiny::shinyApp(app_ui(), app_server), ...)
}

# The path record.js and power-packet.js are served under. Named once because
# launch_app() registers it and app_ui() writes it into the script tags.
www_prefix <- "cyclingpower"
