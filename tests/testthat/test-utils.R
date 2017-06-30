context("utils")


test_that("fars_read works", {
  path_to_2013_data <- system.file("extdata", "accident_2013.csv.bz2",
                                   package = "farsR")

  expect_error(fars_read("no_file_there"), "file 'no_file_there' does not exist")
  expect_is(fars_read(path_to_2013_data), "tbl_df")
  expect_equal(dim(fars_read(path_to_2013_data)), c(30202, 50))
})


test_that("make_filename works", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})


test_that("fars_read_years works", {
  setwd(system.file("extdata", package = "farsR"))

  fars_2014_preprocessed <- tibble::tribble(
    ~MONTH,  ~year,
    11,  2014,
    12,  2014,
    12,  2014,
    12,  2014,
    12,  2014,
    12,  2014
  )
  fars_2014_preprocessed$MONTH <- as.integer(fars_2014_preprocessed$MONTH)

  expect_warning(fars_read_years(3000), "invalid year: 3000")
  expect_is(fars_read_years(2014), "list")
  expect_equal(dim(fars_read_years(2014)[[1]]), c(30056, 2))
  expect_equal(tail(fars_read_years(2014)[[1]]), fars_2014_preprocessed)

  expect_is(fars_read_years(c(2013, 2014)), "list")
  expect_equal(length(fars_read_years(c(2013, 2014))), 2)
  expect_equal(tail(fars_read_years(c(2013, 2014))[[2]]), fars_2014_preprocessed)
})

