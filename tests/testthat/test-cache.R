test_that("ao_cache_dir() returns a real, writable directory", {
  dir <- ao_cache_dir()
  expect_true(dir.exists(dir))

  probe <- file.path(dir, "probe.rds")
  saveRDS(1, probe)
  expect_true(file.exists(probe))
  unlink(probe)
})

test_that("ao_clear_cache() empties the cache directory", {
  dir <- ao_cache_dir()
  saveRDS(1, file.path(dir, "probe2.rds"))

  ao_clear_cache()

  expect_length(list.files(dir), 0)
})
