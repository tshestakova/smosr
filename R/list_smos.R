#' List the BEC-SMOS data files stored on a local computer
#'
#' This function returns a list of the BEC-SMOS data files previously stored on
#' a local computer.
#'
#' This function iterates over all files in the current working directory
#' (default option) or another local folder as indicated by \code{dir} argument
#' and returns a list of the BEC-SMOS data files with the \code{frequency},
#' \code{orbit}, and \code{dates} specified by the user. If no arguments are
#' provided, all BEC-SMOS soil moisture data files found in the selected folder
#' will be listed. A recursive option is also available.
#'
#' @references Pablos M, Gonzalez-Haro C, Portal G, Piles M, Vall-llossera M,
#' Portabella M (2022). SMOS L4 Surface Soil Moisture downscaled maps at 1 km
#' EASE-2 (reprocessed mode) (V.6.0) [Dataset].
#'
#' @param freq an integer specifying temporal frequency of the data. Possible
#' values are: 1 - for daily data, or 3 - for 3-day moving averages, and NULL -
#' for cases when data frequency is irrelevant. Default value is \code{NULL}.
#'
#' @param orbit a character (or character string) specifying SMOS orbit
#' corresponding to the data. Possible values are: ‘a’, ‘asc’, and ‘ascending’ -
#' for an ascending pass, or ‘d’, ‘des’, or ‘descending’ - for a descending
#' pass, and NULL - for cases when orbit is irrelevant. Default value is
#' \code{NULL}.
#'
#' @param dates an object of class \code{Date} or a character string formatted
#' as ‘yyyy-mm-dd’ (e.g. ‘2010-06-01’) which specifies the date(s) to search
#' through. To look for a specific date, it can be a Date object or a character
#' vector of length 1. To iterate over various dates or a time interval, a
#' multiple-element object of class Date or a vector should be passed (e.g. as
#' produced by \code{seq.Date}).
#'
#' @param dir a character string specifying a path to a local directory in which
#' to search for the data. Default value is \code{NULL} which means the dataset
#' is looked up in the current working directory.
#'
#' @param recursive a logical vector indicating whether the listing should
#' recurse into directories. Default is \code{FALSE}.
#'
#' @return a character vector containing full links to the data files on the
#' local computer.
#'
#' @examples
#' \dontrun{
#' # to list all BEC-SMOS data files stored in the current working directory
#' # as well as in the corresponding subfolders
#' smos_files <- list_smos(recursive = TRUE)
#' # to list BEC-SMOS data files with the specified frequency and SMOS orbit
#' # stored in the specified folder
#' smos_files <- list_smos(freq = 3, orbit = "asc", dir = "~/SMOS_data")
#' }
#'
#' @importFrom methods is
#'
#' @export

list_smos <- function(freq = NULL, orbit = NULL, dates = NULL, dir = NULL,
                      recursive = FALSE) {
  if(is.null(dir)) dir <- getwd()
  all_files <- list.files(dir, pattern = ".*(BEC.?SM.*[AD].*[13]d.*.nc).*",
                          full.names = TRUE, recursive = recursive)
  if(is.null(dates)) {
    selected_files <- all_files
  } else
      selected_files <- filter_file_dates(all_files, dates)
  if(!is.null(freq)) {
    selected_files <- filter_file_freq(selected_files, freq)
  }
  if(!is.null(orbit)) {
    selected_files <- filter_file_orbit(selected_files, orbit)
  }
  if(length(selected_files) == 0) {
    stop(simpleError(paste("No files that match the specified parameters",
                           "were found. Modify the search criteria or",
                           "download desired files to proceed.")))
  } else {
      selected_files <- remove_duplicates(selected_files)
      return(selected_files)
  }
}

filter_file_dates <- function(file_list, dates) {
  if(!methods::is(dates, "Date")) dates <- convert_dates(dates)
  dates <- sort(unique(dates))
  selected_files <- vector()
  for(i in 1:length(dates)) {
    date <- dates[i]
    date <- gsub("-", "", date)
    file_names <- sub(".*/", "", file_list)
    selected_file <- grepl(date, file_names)
    selected_file <- file_list[selected_file]
    if(length(selected_file) > 0)
      selected_files <- c(selected_files, selected_file)
  }
  return(selected_files)
}

filter_file_freq <- function(file_list, freq) {
  freq <- check_freq(freq)
  file_count <- length(file_list)
  for(i in 1:file_count) {
    file_name <- sub(".*/", "", file_list[i])
    selected_file <- grep(freq, file_name, value = TRUE)[1]
    if(is.na(selected_file)) file_list[i] <- NA
  }
  file_list <- file_list[!is.na(file_list)]
  return(file_list)
}

filter_file_orbit <- function(file_list, orbit) {
  orbit <- check_orbit(orbit)
  file_count <- length(file_list)
  for(i in 1:file_count) {
    file_name <- sub(".*/", "", file_list[i])
    selected_file <- grep(orbit, file_name, value = TRUE)[1]
    if(is.na(selected_file)) file_list[i] <- NA
  }
  file_list <- file_list[!is.na(file_list)]
  return(file_list)
}

remove_duplicates <- function(file_list) {
  file_name <- sub(".*/", "", file_list)
  file_list <- file_list[!duplicated(file_name)]
  return(file_list)
}
