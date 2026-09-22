test_that("ao_area_boundary() fetches a real boundary", {
  skip_if_offline()
  skip_on_cran()

  boundary <- ao_area_boundary("lad", "E08000035") # Leeds

  expect_s3_class(boundary, "sf")
  expect_equal(nrow(boundary), 1)
  expect_true(boundary$LAD25NM[1] == "Leeds")
})
