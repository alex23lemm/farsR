#' Summarize FARS report accidents by month and year in a tibble
#'
#' \code{fars_read_years()} creates a summary of several FARS reports showing
#' monthly accidents by year and stores the result in a single tibble.
#'
#' \code{fars_summarize_years()} expects to find the report files in the
#' current working directory.
#'
#' @inheritParams fars_read_years
#' @return A tibble conataining the summarized FARS accident information.
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
