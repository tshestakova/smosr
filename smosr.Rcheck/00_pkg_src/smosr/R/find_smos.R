#' Find BEC-SMOS soil moisture data in Barcelona Expert Center (BEC) server
#'
#' This function searches for BEC-SMOS soil moisture data available on Barcelona
#' Expert Center (BEC) server for the frequency, orbit, and dates specified by
#' the user.
#'
#' BEC-SMOS soil moisture (SM) data is a regional root zone SM product that
#' covers Europe and Mediterranean countries. Particularly, \code{smosr} package
#' works with the reprocessed level-4 (L4) SM estimates downscaled to ~1 km
#' spatial resolution (EASE-grid v.2). The data is computed for two time periods
#' (argument \code{frequency}): daily and 3-day moving averages produced by a
#' temporal aggregation of the daily products. Note that SMOS ascending and
#' descending passes (argument \code{orbit}) are processed separately. The
#' BEC-SMOS SM product is available starting from June 1st, 2010 throughout the
#' end of 2022. The currently supported version is 6.0. For more details about
#' the BEC-SMOS soil moisture products, see the technical note available at
#' \url{https://digital.csic.es/handle/10261/303808}.
#'
#' Note that this function requires a username and a password to access the BEC
#' server. Pass your credentials in using the
#' \code{\link[=set_credentials]{set_credentials()}} function. Otherwise, if you
#' do not have your BEC login details yet, please register on
#' \url{https://bec.icm.csic.es/bec-ftp-service-registration}.
#'
#'
#' @param freq an integer specifying temporal frequency of the data. Possible
#' values are: 1 - for daily data, or 3 - for 3-day moving averages. No
#' default value is provided.
#'
#' @param orbit a character (or character string) specifying SMOS orbit
#' corresponding to the data. Possible values are: ‘a’, ‘asc’, and ‘ascending’ -
#' for an ascending pass, or ‘d’, ‘des’, or ‘descending’ - for a descending
#' pass. No default value is provided.
#'
#' @param dates an object of class \code{Date} or a character string formatted
#' as ‘yyyy-mm-dd’ (e.g. ‘2010-06-01’) which specifies the date(s) to search
#' through. To look for a specific date, it can be a Date object or a character
#' vector of length 1. To iterate over various dates or a time interval, a
#' multiple-element object of class Date or a vector should be passed (e.g. as
#' produced by \code{seq.Date}).
#'
#' @return a character vector containing full links to the data files on the
#' server.
#'
#' @examples
#' \dontrun{
#' # to look for SMOS data on a specific date
#' smos_data <- find_smos(freq = 1, orbit = "a", dates = "2022-12-31")
#' # to search over a date range
#' start_date <- as.Date("2022-01-01")
#' end_date <- as.Date("2022-12-31")
#' date_range <- seq(start_date, end_date, by = 10)
#' smos_data <- find_smos(freq = 3, orbit = "descending", dates = date_range)
#' }
#'
#' @importFrom lubridate year
#' @importFrom methods is
#' @importFrom RCurl curlVersion
#' @importFrom RCurl getURL
#'
#' @export

find_smos <- function(freq, orbit, dates) {
  check_rcurl()
  check_creds()
  if(missing(freq))
    stop(simpleError("Argument 'freq' is missing, with no default."))
  freq <- check_freq(freq)
  if(missing(orbit))
    stop(simpleError("Argument 'orbit' is missing, with no default."))
  orbit <- check_orbit(orbit)
  if(missing(dates))
    stop(simpleError("Argument 'dates' is missing, with no default."))
  if(!methods::is(dates, "Date")) dates <- convert_dates(dates)
  check_dates(dates)
  main_folder <- set_main_dir(freq, orbit)
  file_paths <- set_file_path(main_folder, dates)
  return(file_paths)
}

check_rcurl <- function() {
  curl_version <- RCurl::curlVersion()
  if(!"sftp" %in% curl_version$protocols)
    stop(simpleError(paste("The current version of RCurl library",
                            "does not support SFTP protocol. Run",
                            "'Rcurl::curlVersion()' for more details.")))
}

check_creds <- function() {
  if(Sys.getenv("userpass") == "" || Sys.getenv("userpass") == ":")
    stop(simpleError(paste("Username and password are required to proceed.",
                           "See 'set_credentials()' for more details.")))
  test_auth <- try(RCurl::getURL(url = "sftp://becftp.icm.csic.es",
                                 port = 27500,
                                 userpwd = Sys.getenv("userpass"),
                                 connecttimeout = 30,
                                 verbose = FALSE,
                                 ftp.use.epsv = FALSE,
                                 .encoding = "UTF-8"),
                                 silent = TRUE)
  if(inherits(test_auth, "try-error") &&
     grepl("Authentication failure", attr(test_auth, "condition")$message))
       stop(simpleError(paste("Username and password are invalid. Use",
                               "'set_credentials()' to update the login",
                               "details.")))
}

check_freq <- function(freq) {
  if(!freq %in% c(1,3))
    stop(simpleError(paste("Argument 'freq' is invalid. It can be either",
                           "daily (i.e. freq = 1) or 3-day averages (i.e.",
                           "freq = 3).")))
  if(freq == 1) {
    freq <- "1d"
  } else freq <- "3d"
}

check_orbit <- function(orbit) {
  allowed_orbit <- c("a", "d", "asc", "des", "ascending", "descending")
  allowed_orbitA <- c("a", "asc", "ascending")
  orbit <- tolower(orbit)
  if(!orbit %in% allowed_orbit)
    stop(simpleError(paste("Argument 'orbit' is invalid. It can be either",
                           "ascending (e.g. orbit = 'asc') or descending",
                           "(e.g. orbit = 'des').")))
  if(orbit %in% allowed_orbitA) {
    orbit <- "A"
  } else orbit <- "D"
}

convert_dates <- function(dates) {
  tryCatch(as.Date(dates), error = function(e)
    stop(simpleError(paste("The submitted dates could not be coerced to a Date",
                           "object. Format your dates as '%Y-%m-%d' or use a",
                           "Date object as input for the 'dates' argument",
                           "(see '?Date' for help).")))
  )
}

check_dates <- function(dates) {
  earliest_date <- as.Date("2010-06-01")
  if(all(dates < earliest_date))
    stop(simpleError("Search dates cannot precede June 1, 2010."))
  latest_date <- as.Date("2022-12-31")
  if(all(dates > latest_date))
    stop(simpleError("Search dates cannot exceed December 31, 2022."))
  dates <- sort(unique(dates))
}

set_main_dir <- function(freq, orbit) {
  if(freq == "1d") {
    if(orbit == "A") {
      path <- "/data/LAND/SM/SMOS/Europe_and_Mediterranean/v6.0/L4/daily/REP/ASC/"
    } else path <- "/data/LAND/SM/SMOS/Europe_and_Mediterranean/v6.0/L4/daily/REP/DES/"
  } else if(orbit == "A") {
      path <- "/data/LAND/SM/SMOS/Europe_and_Mediterranean/v6.0/L4/3days/REP/ASC/"
  } else path <- "/data/LAND/SM/SMOS/Europe_and_Mediterranean/v6.0/L4/3days/REP/DES/"
}

set_file_path <- function(main_folder, dates) {
  years <- unique(lubridate::year(dates))
  dates_count <- length(dates)
  file_count <- dates_count
  file_paths <- vector()
  file_missing <- NA
  for(i in years) {
    main_path_year <- paste0(main_folder, i, "/")
    dates_for_year <- dates[lubridate::year(dates) == i]
    dates_for_year <- gsub("-", "", dates_for_year)
    file_list <- sftp_file_list(main_path_year)
    for(j in dates_for_year) {
      n <- length(file_paths) + 1
      file_name <- grep(j, file_list, value = TRUE)[1]
      if(is.na(file_name)) {
        file_count <- file_count - 1
        if(is.na(file_missing)) {
          file_missing <- sub('(\\d{2})(\\d{2})$', '-\\1-\\2', j)
        } else file_missing <- paste(file_missing,
                                     sub('(\\d{2})(\\d{2})$',
                                     '-\\1-\\2', j))
      } else file_paths[n] <- paste0("sftp://becftp.icm.csic.es:27500",
                                     main_path_year, file_name)
    }
  }
  if(length(file_paths) == 0)
    stop(simpleError(paste("No data that match the specified parameters were",
                           "found on the server. Modify the search criteria",
                           "and try again.")))
  if(file_count == dates_count) {
    message("Done. All requested files were successfully found.")
  } else message(paste("Done.", file_count, "out of", dates_count,
                       "requested files were found. Run 'missing_smos()'",
                       "to check which dates the files are missing for."))
  Sys.setenv(missing = file_missing)
  return(file_paths)
}

sftp_file_list <- function(main_path_year) {
  url = paste0("sftp://becftp.icm.csic.es", main_path_year)
  raw_list <- RCurl::getURL(url = url,
                            port = 27500,
                            userpwd = Sys.getenv("userpass"),
                            connecttimeout = 30,
                            verbose = FALSE,
                            ftp.use.epsv = FALSE,
                            .encoding = "UTF-8",
                            ftplistonly = TRUE)
  clean_list <- strsplit(raw_list, "\n", fixed = TRUE)[[1]]
  files_found <- clean_list[!clean_list %in% c(".", "..")]
  return(files_found)
}
