#' Local cache directory for downloaded ONS lookup tables
#'
#' @return A file path, created if it doesn't already exist.
#' @keywords internal
ao_cache_dir <- function() {
  dir <- tools::R_user_dir("AreasOfOpportunity", "cache")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' Read a value from the cache, computing and storing it if missing
#'
#' @param key Cache key; becomes the `.rds` filename.
#' @param compute A zero-argument function that produces the value to cache.
#'
#' @return The cached (or freshly computed) value.
#' @keywords internal
ao_cached <- function(key, compute) {
  path <- file.path(ao_cache_dir(), paste0(key, ".rds"))
  if (file.exists(path)) {
    return(readRDS(path))
  }
  value <- compute()
  saveRDS(value, path)
  value
}

#' Clear the local ONS lookup-table cache
#'
#' Deletes cached lookup tables downloaded by [ao_areas()]. Boundaries are
#' never cached (they're fetched fresh per call), so this only affects
#' lookup performance, not boundary data.
#'
#' @return `TRUE`, invisibly.
#' @export
ao_clear_cache <- function() {
  unlink(list.files(ao_cache_dir(), full.names = TRUE))
  invisible(TRUE)
}
