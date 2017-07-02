#' Plot all accident locations for a given year on a US federal state map
#'
#' \code{fars_map_state()} consumes the informaton stored in a single FARS report
#' and creates a plot showing the location of all accidents for a specified US
#' federal state and year.
#'
#' This function has no return value. It is only used for its plotting side effects.
#' A message will be printed to the console if no accidents were reported for a given
#' and valid state-year combination. \code{fars_map_state()} expects to find
#' the report file in the current working directory.
#'
#' @param state.num Number representing a US federal state.
#' @param year Input vector specifying a single year for which the accidents should be plotted.
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(13, 2014)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  if (!require(maps))
    stop("the 'maps' package needs to be installed first")

  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
