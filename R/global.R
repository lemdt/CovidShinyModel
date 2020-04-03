#' Enable bookmarking mode, using values from the URL, if present.
#'
#' @export
start_app <- function() {
  shiny::addResourcePath("covidshiny", system.file("assets", package = "covidshiny"))
  shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Run covidshiny::start_app() to launch the app")
}
