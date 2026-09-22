#' Fetch and cache one bridging lookup table
#'
#' @param lookup_name Name of an entry in [ons_lookups].
#' @return A tibble of the raw ONS lookup, from cache after the first call.
#' @keywords internal
get_lookup <- function(lookup_name) {
  spec <- ons_lookups[[lookup_name]]
  ao_cached(
    paste0("lookup_", lookup_name),
    function() arcgis_query_all(spec$service, spec$fields)
  )
}

#' Build the joined area hierarchy, one tibble per level
#'
#' Each level's tibble has a `code` and `name` column for that level, plus a
#' `<ancestor>_code`/`<ancestor>_name` column pair for every coarser level it
#' can be traced up to (see [ao_area_levels()] for how the levels relate).
#'
#' @return A named list of tibbles, one per level in [ao_area_levels()].
#' @keywords internal
area_universe <- function() {
  ao_cached("area_universe", function() {
    oa_lsoa_msoa <- get_lookup("oa_lsoa_msoa")
    lsoa_ward_lad <- get_lookup("lsoa_ward_lad")
    ward_lad_county <- get_lookup("ward_lad_county")
    lad_csp_pfa <- get_lookup("lad_csp_pfa")

    lad_tbl <- ward_lad_county |>
      dplyr::distinct(.data$LAD25CD, .data$LAD25NM, .data$CTYUA25CD, .data$CTYUA25NM) |>
      dplyr::rename(code = "LAD25CD", name = "LAD25NM", county_code = "CTYUA25CD", county_name = "CTYUA25NM") |>
      dplyr::left_join(
        lad_csp_pfa |>
          dplyr::distinct(.data$LAD24CD, .data$CSP24CD, .data$CSP24NM, .data$PFA24CD, .data$PFA24NM) |>
          dplyr::rename(
            code = "LAD24CD", csp_code = "CSP24CD", csp_name = "CSP24NM",
            police_force_area_code = "PFA24CD", police_force_area_name = "PFA24NM"
          ),
        by = "code"
      )

    ward_tbl <- ward_lad_county |>
      dplyr::distinct(.data$WD25CD, .data$WD25NM, .data$LAD25CD, .data$LAD25NM, .data$CTYUA25CD, .data$CTYUA25NM) |>
      dplyr::rename(
        code = "WD25CD", name = "WD25NM", lad_code = "LAD25CD", lad_name = "LAD25NM",
        county_code = "CTYUA25CD", county_name = "CTYUA25NM"
      ) |>
      dplyr::left_join(
        lad_tbl |> dplyr::select("code", "csp_code", "csp_name", "police_force_area_code", "police_force_area_name"),
        by = c("lad_code" = "code")
      )

    lsoa_names <- oa_lsoa_msoa |> dplyr::distinct(.data$LSOA21CD, .data$LSOA21NM)

    lsoa_tbl <- lsoa_ward_lad |>
      dplyr::distinct(.data$LSOA21CD, .data$WD25CD, .data$WD25NM, .data$LAD25CD, .data$LAD25NM) |>
      dplyr::left_join(lsoa_names, by = "LSOA21CD") |>
      dplyr::rename(
        code = "LSOA21CD", name = "LSOA21NM", ward_code = "WD25CD", ward_name = "WD25NM",
        lad_code = "LAD25CD", lad_name = "LAD25NM"
      ) |>
      dplyr::left_join(
        ward_tbl |> dplyr::select(
          "code", "county_code", "county_name", "csp_code", "csp_name",
          "police_force_area_code", "police_force_area_name"
        ),
        by = c("ward_code" = "code")
      )

    oa_tbl <- oa_lsoa_msoa |>
      dplyr::distinct(.data$OA21CD, .data$LSOA21CD) |>
      dplyr::rename(code = "OA21CD", lsoa_code = "LSOA21CD") |>
      dplyr::mutate(name = NA_character_) |>
      dplyr::left_join(
        lsoa_tbl |> dplyr::rename(lsoa_name = "name") |> dplyr::select(
          "code", "lsoa_name", "ward_code", "ward_name", "lad_code", "lad_name",
          "county_code", "county_name", "csp_code", "csp_name",
          "police_force_area_code", "police_force_area_name"
        ),
        by = c("lsoa_code" = "code")
      )

    county_tbl <- ward_lad_county |>
      dplyr::distinct(.data$CTYUA25CD, .data$CTYUA25NM) |>
      dplyr::rename(code = "CTYUA25CD", name = "CTYUA25NM")

    csp_tbl <- lad_csp_pfa |>
      dplyr::distinct(.data$CSP24CD, .data$CSP24NM, .data$PFA24CD, .data$PFA24NM) |>
      dplyr::rename(
        code = "CSP24CD", name = "CSP24NM",
        police_force_area_code = "PFA24CD", police_force_area_name = "PFA24NM"
      )

    pfa_tbl <- lad_csp_pfa |>
      dplyr::distinct(.data$PFA24CD, .data$PFA24NM) |>
      dplyr::rename(code = "PFA24CD", name = "PFA24NM")

    list(
      output_area = oa_tbl,
      lsoa = lsoa_tbl,
      ward = ward_tbl,
      lad = lad_tbl,
      county = county_tbl,
      csp = csp_tbl,
      police_force_area = pfa_tbl
    )
  })
}

#' Validate a level argument
#'
#' @param level A single string.
#' @return `level`, invisibly, if valid; errors otherwise.
#' @keywords internal
check_level <- function(level) {
  levels <- ao_area_levels()
  if (!is.character(level) || length(level) != 1 || !(level %in% levels)) {
    stop(
      "`level` must be one of: ", paste(levels, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(level)
}

#' The UK area levels this package understands
#'
#' The finest four levels form a strict chain - each Output Area sits inside
#' one LSOA, each LSOA inside one Ward, each Ward inside one Local Authority
#' District (`"lad"`). `"county"`, `"csp"` (Community Safety Partnership) and
#' `"police_force_area"` are all derived independently *from* `"lad"` rather
#' than nested inside `"county"` - except that `"csp"` also nests inside
#' `"police_force_area"` (each police force area is made up of several
#' community safety partnerships).
#'
#' Coverage: these levels come from ONS geographies that are complete for
#' England and Wales. `"lad"` and `"ward"` also extend to Scotland and
#' Northern Ireland, but `"county"`, `"csp"` and `"police_force_area"` do
#' not, so cross-level filtering only works reliably within England & Wales.
#'
#' @return A character vector of level names, from finest to coarsest.
#' @export
#'
#' @examples
#' ao_area_levels()
ao_area_levels <- function() {
  c("output_area", "lsoa", "ward", "lad", "county", "csp", "police_force_area")
}

#' List or search UK areas at a given level
#'
#' Areas are fetched from the ONS Open Geography Portal's lookup tables the
#' first time any `ao_areas()`/[ao_area_boundary()] call needs them, and
#' cached locally afterwards (see [ao_clear_cache()]).
#'
#' @param level One of [ao_area_levels()].
#' @param name Optional substring to match against area names
#'   (case-insensitive). Not supported for `"output_area"`, which has no
#'   names.
#' @param code Optional substring to match against area codes
#'   (case-insensitive).
#' @param within_level Optional coarser level in [ao_area_levels()] to
#'   restrict results to.
#' @param within_code Required alongside `within_level`: the code of the
#'   specific `within_level` area to restrict to.
#'
#' @return A tibble with (at least) `code` and `name` columns for matching
#'   areas, plus one `<ancestor>_code`/`<ancestor>_name` column pair per
#'   coarser level it's traceable to.
#' @export
#'
#' @examples
#' \dontrun{
#' ao_areas("lad", name = "Leeds")
#' leeds_code <- ao_areas("lad", name = "Leeds")$code[1]
#' ao_areas("ward", within_level = "lad", within_code = leeds_code)
#' }
ao_areas <- function(level, name = NULL, code = NULL, within_level = NULL, within_code = NULL) {
  check_level(level)
  tbl <- area_universe()[[level]]

  if (!is.null(within_level)) {
    check_level(within_level)
    if (is.null(within_code)) {
      stop("`within_code` must be supplied together with `within_level`.", call. = FALSE)
    }
    within_col <- paste0(within_level, "_code")
    if (!within_col %in% names(tbl)) {
      stop(
        sprintf("'%s' areas can't be filtered by '%s' - it isn't a coarser level for them.", level, within_level),
        call. = FALSE
      )
    }
    tbl <- tbl[!is.na(tbl[[within_col]]) & tbl[[within_col]] == within_code, ]
  }

  if (!is.null(code)) {
    tbl <- tbl[grepl(code, tbl$code, ignore.case = TRUE), ]
  }

  if (!is.null(name)) {
    if (!"name" %in% names(tbl) || all(is.na(tbl$name))) {
      stop(sprintf("'%s' areas don't have names to filter by.", level), call. = FALSE)
    }
    tbl <- tbl[!is.na(tbl$name) & grepl(name, tbl$name, ignore.case = TRUE), ]
  }

  tibble::as_tibble(tbl)
}
