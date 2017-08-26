#' Read file as tibble
#'
#' Check whether file exists first, if it does, read the
#' the file then convert it to tibble format. Otherwise error message will return.
#'
#' @param filename path of the file to be read
#' @return tibble contain data in the file
#' @import dplyr readr
#' @examples
#'  fars_read("data.csv")
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate filename with provided year
#'
#'  Provide a year, this functon will generate the filename with pre-set format
#'  using the year provided. Thin input must be able to converted to integer or
#'  an error will occur.
#'
#' @param year a number or string represented the year that need to included in the filename.
#' @return a string, represented the file name with pre-set format
#' @examples
#' # all examples below generate same output
#'  make_filename(2014)
#'  make_filename("2014")
#'  make_filename(2014.3) #not recommended
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Extract data according to year
#'
#' The function take a list of years as input, use make_filename functon, and
#' fars_read to read data from disk according to the years provided. Only month
#' and year columns will be kept. Final output will be list format each element
#' contain data for one year.
#'
#' @param years a list contain years use to extract data, years can either denote as
#' integers or strings.
#' @import dplyr
#' @return a list contain only the month and year provided, each element contain
#' data for the same year. If the no data match the year provided NULL and a warning
#' message will return.
#' @examples
#' years <- list(2014,"2015")
#' fars_read_years(years)
#' @export

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

#' summarize for years and month
#'
#' summarize number count for each month and year combination
#'
#' @param years a list of years used for \code{fars_read_years} function.
#' @return a table in tibble format, contain couts for each month-year combination.
#' @import dplyr tidyr
#' @examples
#' years <- list("2014", 2015)
#' far_summrize_years(years)
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot state data on the map
#'
#' take state.num and year as input, extract longitude and latitude accordingly,
#' then plot the state on the map. Error will occur if state number is invalid.
#'
#' @param state.num state number according to which state is selected
#' @param year The year to be selected.  This can be either string or numberic
#' @import dplyr tidyr
#' @return a graph with each selected observation ploted on the map
#' @examples
#' fars_map_state(1, 2015)
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
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
