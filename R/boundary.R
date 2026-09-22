#' Fetch the boundary polygon for one area
#'
#' @param level One of [ao_area_levels()].
#' @param code The area's code, e.g. from [ao_areas()].
#'
#' @return An `sf` object with one row: the area's `code` (and `name`, where
#'   that level has one) and its polygon geometry (longitude/latitude,
#'   EPSG:4326).
#' @export
#'
#' @examples
#' \dontrun{
#' leeds_code <- ao_areas("lad", name = "Leeds")$code[1]
#' ao_area_boundary("lad", leeds_code)
#' }
ao_area_boundary <- function(level, code) {
  check_level(level)
  spec <- ons_boundaries[[level]]
  out_fields <- c(spec$code_field, if (!is.na(spec$name_field)) spec$name_field)
  arcgis_query_sf(spec$service, spec$code_field, code, out_fields = out_fields)
}
