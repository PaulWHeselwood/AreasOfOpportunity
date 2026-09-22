circle_boundary <- function(n = 100) {
  theta <- seq(0, 2 * pi, length.out = n)
  coords <- cbind(lng = cos(theta), lat = sin(theta))
  coords[n, ] <- coords[1, ]
  poly <- sf::st_polygon(list(coords))
  sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 4326))
}

test_that("ao_data_sources() returns the expected sources", {
  expect_identical(ao_data_sources(), c("crime", "stop_search"))
})

test_that("ao_data() errors clearly on an invalid source", {
  expect_error(ao_data("weather", "lad", "E08000035"), "must be one of")
})

test_that("boundary_to_poly_string() produces well-formed lat,lng pairs", {
  poly <- boundary_to_poly_string(circle_boundary(), max_chars = 100000)

  expect_match(poly, "^-?[0-9.]+,-?[0-9.]+(:-?[0-9.]+,-?[0-9.]+)*$")
  expect_gt(lengths(regmatches(poly, gregexpr(":", poly))), 4)
})

test_that("boundary_to_poly_string() thins points to respect max_chars", {
  full <- boundary_to_poly_string(circle_boundary(), max_chars = 100000)
  thinned <- boundary_to_poly_string(circle_boundary(), max_chars = 100)

  n_points <- function(x) lengths(regmatches(x, gregexpr(":", x))) + 1
  expect_lt(n_points(thinned), n_points(full))
  expect_gte(n_points(thinned), 4)
})

test_that("expand_months() handles NULL, a single date, and a range", {
  expect_identical(expand_months(NULL), list(NULL))
  expect_identical(expand_months("2024-01"), list("2024-01"))
  expect_identical(
    expand_months(c("2023-11", "2024-02")),
    list("2023-11", "2023-12", "2024-01", "2024-02")
  )
})

test_that("expand_months() rejects a reversed range", {
  expect_error(expand_months(c("2024-02", "2024-01")), "on or before")
})

test_that("ao_data() fetches real crime data for an area", {
  skip_if_offline()
  skip_on_cran()

  crimes <- ao_data("crime", "lad", "E08000035", date = "2024-01") # Leeds

  expect_s3_class(crimes, "tbl_df")
  expect_gt(nrow(crimes), 0)
  expect_true(all(c("level", "code", "category", "latitude", "longitude") %in% names(crimes)))
})
