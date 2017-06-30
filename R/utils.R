#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Read a compressed csv report file downloaded from the Fatality
#' Analysis Reporting System (FARS) into a tibble
#'
#' \code{fars_read()} reads a locally stored and compressed csv file downloaded from the
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System}
#' into a tibble.
#'
#' @param filename Path to a file containing FARS data.
#' @return A tibble. If the file cannot be found at the specified path,
#' an error is thrown.
#' @examples
#' \dontrun{
#' fars_complete_2013 <- fars_read("accident_2013.csv.bz2")
#' }
fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create valid file name for a compressed FARS report for a given year
#'
#' @param year Input vector specifying a single year.
#' @return A character vector containing the complete file name of a compressed
#' FARS report.
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read specific information of multiple FARS report files into a list of tibbles
#'
#' \code{fars_read_years()} reads several locally stored and compressed FARS report
#'  files for the specified years into a list of tibbles.
#'
#' Only the monthly and yearly information of each accident included in an
#' individual FARS report is stored in the respective tibble reducing each original
#' data set to two columns. \code{fars_read_years()} expects to find the
#' report files in the current working directory.
#'
#' @param years Input vector or list specifying one or several years in character or
#' integer format.
#' @return A list of tibbles. Each tibble contains just two columns of the original
#' FARS data set (\code{MONTH} and \code{year}). Each row corresponds to a single accident.
#' @examples
#' \dontrun{
#' fars2013 <- fars_read_years(2013)
#' fars2013 <- fars_read_years("2013")
#' fars1314 <- fars_read_years(c(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

