#' Map a single area's boundary
#'
#' @param level One of [ao_area_levels()].
#' @param code The area's code, e.g. from [ao_areas()].
#' @param title Optional plot title. Defaults to the area's name (or its
#'   code, for levels like `"output_area"` that have no name).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' leeds_code <- ao_areas("lad", name = "Leeds")$code[1]
#' ao_plot_area("lad", leeds_code)
#' }
ao_plot_area <- function(level, code, title = NULL) {
  boundary <- ao_area_boundary(level, code)

  if (is.null(title)) {
    name_field <- ons_boundaries[[level]]$name_field
    title <- if (!is.na(name_field) && name_field %in% names(boundary)) {
      boundary[[name_field]][1]
    } else {
      code
    }
  }

  ggplot2::ggplot(boundary) +
    ggplot2::geom_sf(fill = "#2c3e50", alpha = 0.15, colour = "#2c3e50") +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal()
}

#' Map a single area highlighted within England & Wales
#'
#' Like [ao_plot_area()], but draws the area against a full England & Wales
#' outline for national context, rather than on its own.
#'
#' @param level One of [ao_area_levels()].
#' @param code The area's code, e.g. from [ao_areas()].
#' @param title Optional plot title. Defaults to the area's name (or its
#'   code, for levels like `"output_area"` that have no name).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' leeds_code <- ao_areas("lad", name = "Leeds")$code[1]
#' ao_plot_area_in_context("lad", leeds_code)
#' }
ao_plot_area_in_context <- function(level, code, title = NULL) {
  background <- england_wales_outline()
  boundary <- ao_area_boundary(level, code)

  if (is.null(title)) {
    name_field <- ons_boundaries[[level]]$name_field
    title <- if (!is.na(name_field) && name_field %in% names(boundary)) {
      boundary[[name_field]][1]
    } else {
      code
    }
  }

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = background, fill = "#f5f5f5", colour = "#cccccc") +
    ggplot2::geom_sf(data = boundary, fill = "#e63946", colour = "#e63946", alpha = 0.7) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
