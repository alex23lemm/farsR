context("fars_map_state")

test_that("fars_map_state works", {
  setwd(system.file("extdata", package = "farsR"))

  expect_null(fars_map_state(13, 2015))
  expect_error(fars_map_state(15, 2015), "nothing to draw: all regions out of bounds")
  expect_error(fars_map_state(63, 2013), "invalid STATE number: 63")
})
