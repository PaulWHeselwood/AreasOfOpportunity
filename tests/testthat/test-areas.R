test_that("ao_area_levels() returns the expected levels in hierarchy order", {
  expect_identical(
    ao_area_levels(),
    c("output_area", "lsoa", "ward", "lad", "county", "csp", "police_force_area")
  )
})

test_that("ao_areas() errors clearly on an invalid level", {
  expect_error(ao_areas("postcode"), "must be one of")
})

test_that("ao_areas() requires within_code alongside within_level", {
  expect_error(
    ao_areas("ward", within_level = "lad"),
    "within_code"
  )
})

test_that("ao_areas() rejects within_level = 'csp' for ward/lad (not a clean nesting)", {
  skip_if_offline()
  skip_on_cran()

  expect_error(
    ao_areas("ward", within_level = "csp", within_code = "E22000342"),
    "isn't a coarser level"
  )
})
