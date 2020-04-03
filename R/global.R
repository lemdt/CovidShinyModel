#' Enable bookmarking mode, using values from the URL, if present.
#'
#' @import shiny
#' @export
start_app <- function() {
  enableBookmarking("url")
  shinyApp(ui = ui, server = server)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Run covidshiny::start_app() to launch the app")
}
