library(runkeepR)
context("Data can be loaded")

test_that("example data can be found and loaded correctly", {
  expect_silent(example_routes <- load_tracks(system.file("extdata", package="runkeepR")))
  expect_is(example_routes, "data.frame")
  expect_equal(nrow(example_routes), 374L)
  expect_is(example_routes$time, "POSIXct")
})