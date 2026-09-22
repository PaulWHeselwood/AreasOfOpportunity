#' Tag a data-source result with the area it was fetched for
#'
#' @param tbl A tibble from a data-source connector.
#' @param level One of [ao_area_levels()].
#' @param code The area's code.
#'
#' @return `tbl` with `level`/`code` columns added at the front, or `tbl`
#'   unchanged if it has no rows.
#' @keywords internal
tag_area <- function(tbl, level, code) {
  if (nrow(tbl) == 0) {
    return(tbl)
  }
  tbl$level <- level
  tbl$code <- code
  dplyr::relocate(tbl, "level", "code")
}

#' Registry of data sources [ao_data()] can fetch
#'
#' Each entry is a `list(fetch = function(level, code, ...) ...)`. Adding a
#' new data source means adding one entry here - [ao_data()] itself doesn't
#' need to change. A function rather than a plain list so it doesn't depend
#' on file-sourcing order within the package.
#'
#' @keywords internal
data_source_registry <- function() {
  list(
    crime = list(fetch = fetch_crime),
    stop_search = list(fetch = fetch_stop_search)
  )
}

#' The data sources this package can fetch
#'
#' @return A character vector of source names, for use as `source` in
#'   [ao_data()].
#' @export
#'
#' @examples
#' ao_data_sources()
ao_data_sources <- function() {
  names(data_source_registry())
}

#' Fetch a dataset for one area
#'
#' A single generic entry point for every data source this package knows
#' about (see [ao_data_sources()]) - each returns a tibble tagged with the
#' `level`/`code` of the area it was fetched for.
#'
#' @param source One of [ao_data_sources()].
#' @param level One of [ao_area_levels()].
#' @param code The area's code, e.g. from [ao_areas()].
#' @param ... Passed on to the data source's connector. For `"crime"`:
#'   `date` (a single month as `"YYYY-MM"`, or `NULL` for the latest month)
#'   and `category` (defaults to `"all-crime"`). For `"stop_search"`:
#'   `date`.
#'
#' @return A tibble of results for that area, tagged with `level`/`code`
#'   columns, or an empty tibble if nothing was found.
#' @export
#'
#' @examples
#' \dontrun{
#' leeds_code <- ao_areas("lad", name = "Leeds")$code[1]
#' ao_data("crime", "lad", leeds_code, date = "2024-01")
#' ao_data("stop_search", "lad", leeds_code, date = "2024-01")
#' }
ao_data <- function(source, level, code, ...) {
  if (!is.character(source) || length(source) != 1 || !(source %in% ao_data_sources())) {
    stop("`source` must be one of: ", paste(ao_data_sources(), collapse = ", "), ".", call. = FALSE)
  }
  check_level(level)
  data_source_registry()[[source]]$fetch(level, code, ...)
}
