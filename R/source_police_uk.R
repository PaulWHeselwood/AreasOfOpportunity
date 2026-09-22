#' The police.uk open data API
#'
#' @keywords internal
police_uk_host <- "https://data.police.uk/api"

#' Turn an area boundary into a police.uk `poly` query value
#'
#' police.uk's street-level endpoints take a custom search area as
#' `lat,lng:lat,lng:...`. Our boundaries can be `MULTIPOLYGON`s with holes
#' (e.g. coastal Local Authority Districts), so this takes the boundary's
#' convex hull first - trading a little over-inclusion in concave notches
#' for a single simple ring of points - then rounds and thins points until
#' the resulting string fits `max_chars` (police.uk's request-line limit
#' sits around 4094 characters total; keeping the `poly` value itself under
#' ~2500 leaves comfortable headroom for the rest of the URL).
#'
#' @param boundary An `sf` object, as returned by [ao_area_boundary()].
#' @param max_chars Maximum length, in characters, of the resulting string.
#' @param dp Decimal places to round coordinates to (5dp is ~1.1m precision).
#'
#' @return A single string in `lat,lng:lat,lng:...` format.
#' @keywords internal
boundary_to_poly_string <- function(boundary, max_chars = 2500, dp = 5) {
  hull <- sf::st_convex_hull(sf::st_union(boundary))
  coords <- sf::st_coordinates(hull)
  pts <- data.frame(lat = round(coords[, "Y"], dp), lng = round(coords[, "X"], dp))

  poly_string <- function(df) paste(sprintf("%s,%s", df$lat, df$lng), collapse = ":")

  thinned <- pts
  step <- 1
  while (nchar(poly_string(thinned)) > max_chars && nrow(thinned) > 4) {
    step <- step + 1
    idx <- unique(c(seq(1, nrow(pts), by = step), nrow(pts)))
    thinned <- pts[idx, ]
  }
  poly_string(thinned)
}

#' Query a police.uk street-level (poly-based) endpoint for one area
#'
#' @param path The endpoint path, e.g. `"stops-street"` or
#'   `"crimes-street/all-crime"`.
#' @param level One of [ao_area_levels()].
#' @param code The area's code, e.g. from [ao_areas()].
#' @param date A single month as `"YYYY-MM"`, or `NULL` for the latest month
#'   available.
#' @param max_chars Passed to [boundary_to_poly_string()]. If the request
#'   still gets a 400 at this size, it's retried once with half as many
#'   characters before giving up.
#'
#' @return A tibble, with the nested `location` field flattened into
#'   `latitude`/`longitude`/`street_name` columns, or an empty
#'   `tibble::tibble()` if nothing was found.
#' @keywords internal
police_uk_query_poly <- function(path, level, code, date = NULL, max_chars = 2500) {
  boundary <- ao_area_boundary(level, code)

  do_request <- function(poly_value) {
    req <- httr2::request(paste0(police_uk_host, "/", path))
    req <- httr2::req_url_query(req, poly = poly_value, date = date)
    httr2::req_perform(req)
  }

  poly <- boundary_to_poly_string(boundary, max_chars = max_chars)
  resp <- tryCatch(do_request(poly), httr2_http_400 = function(e) NULL)

  if (is.null(resp)) {
    if (max_chars <= 800) {
      stop("Request to police.uk API failed with a 400 error even after simplifying the boundary.", call. = FALSE)
    }
    poly <- boundary_to_poly_string(boundary, max_chars = max_chars / 2)
    resp <- do_request(poly)
  }

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (length(body) == 0) {
    return(tibble::tibble())
  }

  out <- tibble::as_tibble(body)
  if ("location" %in% names(out)) {
    loc <- out$location
    out$latitude <- as.numeric(loc$latitude)
    out$longitude <- as.numeric(loc$longitude)
    out$street_name <- if ("street" %in% names(loc)) loc$street$name else NA_character_
    out$location <- NULL
  }
  out
}

#' @keywords internal
fetch_crime <- function(level, code, date = NULL, category = "all-crime") {
  out <- police_uk_query_poly(paste0("crimes-street/", category), level, code, date = date)
  tag_area(out, level, code)
}

#' @keywords internal
fetch_stop_search <- function(level, code, date = NULL) {
  out <- police_uk_query_poly("stops-street", level, code, date = date)
  tag_area(out, level, code)
}
