#' ONS Open Geography Portal service registry
#'
#' Maps each supported area level to its boundary `FeatureServer` and code/
#' name field names, and each hierarchy step to the lookup `FeatureServer`
#' that bridges two levels. All services live under the same ArcGIS host;
#' vintages are pinned here so bumping to a newer ONS release is a one-place
#' edit.
#'
#' @rdname ons_datasets
#' @keywords internal
ons_host <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services"

#' @rdname ons_datasets
#' @keywords internal
ons_boundaries <- list(
  output_area = list(
    service = "Output_Areas_2021_EW_BGC_V2",
    code_field = "OA21CD",
    name_field = NA_character_
  ),
  lsoa = list(
    service = "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4",
    code_field = "LSOA21CD",
    name_field = "LSOA21NM"
  ),
  ward = list(
    service = "WD_DEC_2025_UK_BSC",
    code_field = "WD25CD",
    name_field = "WD25NM"
  ),
  lad = list(
    service = "Local_Authority_Districts_DEC_2025_Boundaries_UK_BUC",
    code_field = "LAD25CD",
    name_field = "LAD25NM"
  ),
  county = list(
    service = "Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BUC",
    code_field = "CTYUA24CD",
    name_field = "CTYUA24NM"
  ),
  csp = list(
    service = "Community_Safety_Partnerships_December_2023_Boundaries_EW_BUC",
    code_field = "CSP23CD",
    name_field = "CSP23NM"
  ),
  police_force_area = list(
    service = "Police_Force_Areas_Dec_2024_EW_BGC",
    code_field = "PFA24CD",
    name_field = "PFA24NM"
  )
)

#' @rdname ons_datasets
#' @keywords internal
ons_lookups <- list(
  oa_lsoa_msoa = list(
    service = "OA_LSOA_MSOA_EW_DEC_2021_LU_v3",
    fields = c("OA21CD", "LSOA21CD", "LSOA21NM", "MSOA21CD", "MSOA21NM")
  ),
  lsoa_ward_lad = list(
    service = "LSOA21_WD25_LAD25_EW_LU_v2",
    fields = c("LSOA21CD", "WD25CD", "WD25NM", "LAD25CD", "LAD25NM")
  ),
  ward_lad_county = list(
    service = "WD25_LAD25_CTYUA25_RGN25_CTRY25_UK_LU",
    fields = c("WD25CD", "WD25NM", "LAD25CD", "LAD25NM", "CTYUA25CD", "CTYUA25NM")
  ),
  lad_csp_pfa = list(
    service = "LAD24_CSP24_PFA24_EW_LU",
    fields = c("LAD24CD", "LAD24NM", "CSP24CD", "CSP24NM", "PFA24CD", "PFA24NM")
  )
)
