#' Launch the interactive area explorer
#'
#' Opens a Shiny app: pick an area type and area, see it highlighted on a
#' map of England & Wales, then pull a summary of crime or stop-and-search
#' outcomes for it over a chosen date range.
#'
#' @return Doesn't return - runs the app until stopped.
#' @export
ao_app <- function() {
  shiny::runApp(system.file("app", package = "AreasOfOpportunity"))
}
