#' Download BEC-SMOS soil moisture data
#'
#' This function automates downloading of BEC-SMOS soil moisture data to a local
#' computer via a secure FTP (SFTP) server.
#'
#' This function downloads the original BEC-SMOS soil moisture data in NetCDF
#' format ("as is") via a secure FTP (SFTP) server. The data files are stored on
#' the local computer in a temporary directory of the current R session (default
#' option) if no otherwise specified by the user.
#'
#' Note that the registration as a user on the Barcelona Expert Center (BEC)
#' webpage is required to access the server. See
#' \code{\link[=set_credentials]{set_credentials()}} for details.
#'
#' @references Pablos M, Gonzalez-Haro C, Portal G, Piles M, Vall-llossera M,
#' Portabella M (2022). SMOS L4 Surface Soil Moisture downscaled maps at 1 km
#' EASE-2 (reprocessed mode) (V.6.0) [Dataset].
#'
#' @param data a character vector as produced by \code{find_smos()} containing
#' external links to the data files on the BEC server.
#'
#' @param dir a character string specifying a path to a local directory in which
#' to save the data. Default value is \code{NULL} meaning that the dataset is
#' stored in a temporary directory of the current R session.
#'
#' @return downloaded files in the specified directory
#'
#' @examples
#' \dontrun{
#' # to download files found with find_smos() into a temporary directory of the current R session
#' start_date <- as.Date("2022-01-01")
#' end_date <- as.Date("2022-12-31")
#' date_range <- seq(start_date, end_date, by = 30)
#' smos_data <- find_smos(freq = 3, orbit = "des", dates = date_range)
#' download_smos(smos_data)
#' }
#'
#' @importFrom RCurl getBinaryURL
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @export

download_smos <- function(data, dir = NULL) {
  if(is.null(dir)) dir <- tempdir()
  if(!file.exists(dir))
    stop(simpleError(paste("Specified directory does not exist. Provide a",
                           "valid path to an existing folder or create a new",
                           "one to proceed.")))
  # old_dir <- getwd()
  # on.exit(setwd(old_dir))
  # setwd(dir)
  userpwd <- Sys.getenv("userpass")
  file_count <- length(data)
  progress_bar <- utils::txtProgressBar(min = 0, max = file_count, initial = 0,
                                        width = 80, style = 3)
  for(i in 1:file_count) {
    file_name <- sub(".*/([0-9:]{4})/", "", data[i])
    file_path <- paste0(dir, "/", file_name)
    file_url <- RCurl::getBinaryURL(data[i], userpwd = userpwd)
    # writeBin(file_url, file_name)
    writeBin(file_url, file_path)
    utils::setTxtProgressBar(progress_bar, i)
  }
  close(progress_bar)
}
