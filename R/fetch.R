#' Escape a value for use in an ArcGIS REST `where` clause
#'
#' @param x A single string.
#' @return `x` with single quotes doubled, ready to wrap in `'...'`.
#' @keywords internal
sql_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

#' Query one page of an ArcGIS `FeatureServer` layer as JSON
#'
#' @param service Service name (relative to [ons_host]).
#' @param where SQL `where` clause.
#' @param out_fields Character vector of fields to return.
#' @param result_offset,result_record_count Pagination parameters.
#'
#' @return The parsed JSON response body (a list), including `features` and
#'   `exceededTransferLimit`.
#' @keywords internal
arcgis_query_json <- function(service, where = "1=1", out_fields = "*",
                               result_offset = NULL, result_record_count = NULL) {
  req <- httr2::request(paste0(ons_host, "/", service, "/FeatureServer/0/query"))
  req <- httr2::req_url_query(
    req,
    where = where,
    outFields = paste(out_fields, collapse = ","),
    outSR = "4326",
    f = "json",
    resultOffset = result_offset,
    resultRecordCount = result_record_count
  )
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Fetch a whole lookup table, paginating as needed
#'
#' @param service Service name (relative to [ons_host]).
#' @param fields Character vector of fields to return.
#' @param page_size Rows requested per page.
#'
#' @return A tibble with one row per feature and one column per field.
#' @keywords internal
arcgis_query_all <- function(service, fields, page_size = 2000) {
  offset <- 0
  pages <- list()
  repeat {
    body <- arcgis_query_json(
      service,
      out_fields = fields,
      result_offset = offset,
      result_record_count = page_size
    )
    feats <- body$features
    n <- if (is.data.frame(feats)) nrow(feats) else length(feats)
    if (is.null(feats) || n == 0) break

    attrs <- feats$attributes
    pages[[length(pages) + 1]] <- tibble::as_tibble(attrs)
    offset <- offset + n

    if (!isTRUE(body$exceededTransferLimit)) break
  }
  dplyr::bind_rows(pages)
}

#' Fetch the boundary of a single area as `sf`
#'
#' @param service Service name (relative to [ons_host]).
#' @param code_field Field holding the area's code.
#' @param code The area's code to match.
#' @param out_fields Character vector of fields to return.
#'
#' @return An `sf` object with one row.
#' @keywords internal
arcgis_query_sf <- function(service, code_field, code, out_fields = "*") {
  where <- sprintf("%s = '%s'", code_field, sql_escape(code))
  req <- httr2::request(paste0(ons_host, "/", service, "/FeatureServer/0/query"))
  req <- httr2::req_url_query(
    req,
    where = where,
    outFields = paste(out_fields, collapse = ","),
    outSR = "4326",
    f = "geojson"
  )

  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  httr2::req_perform(req, path = tmp)
  out <- sf::read_sf(tmp, quiet = TRUE)

  if (nrow(out) == 0) {
    stop(sprintf("No boundary found for %s = '%s'.", code_field, code), call. = FALSE)
  }
  out
}
