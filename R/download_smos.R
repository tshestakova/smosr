#' Download BEC-SMOS soil moisture data
#'
#' This function automates downloading of original BEC-SMOS soil moisture data
#' to a local computer via a secure FTP (SFTP) server.
#'
#' This function downloads the original BEC-SMOS soil moisture data in NetCDF
#' format ("as is") from the BEC server. The data files are stored on the local
#' computer in the current working directory (default option) if no otherwise
#' specified by a user.
#'
#' Note that this function requires a username and a password to access the BEC
#' server. Pass your credentials in using the
#' \code{\link[=set_credentials]{set_credentials()}} function. Otherwise, if you
#' do not have your BEC login details yet, please register on
#' \url{https://bec.icm.csic.es/bec-ftp-service-registration}.
#'
#' @param data a character vector as produced by \code{find_smos()} containing
#' external links to the data files on the BEC server.
#'
#' @param dir a character string specifying a path to a local directory in which
#' to save the data. Default value is \code{NULL} which means the dataset is
#' stored in the current working directory.
#'
#' @examples
#' \dontrun{
#' # to download all files found with find_smos()
#' # into the current working directory
#' start_date <- as.Date("2022-01-01")
#' end_date <- as.Date("2022-12-31")
#' date_range <- seq(start_date, end_date, by = 30)
#' smos_data <- find_smos(freq = 3, orbit = "descending", dates = date_range)
#' download_smos(smos_data)
#' # to download first five items from the complete list of files
#' # and place them in a newly created directory
#' dir.create("~/SMOS_data")
#' download_smos(data = smos_data[1:5], dir = "~/SMOS_data")
#' }
#'
#' @importFrom RCurl getBinaryURL
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @export

download_smos <- function(data, dir = NULL) {
  if(!is.null(dir)) {
    old_dir <- getwd()
    setwd(dir)
  }
  userpwd <- Sys.getenv("userpass")
  file_count <- length(data)
  progress_bar <- txtProgressBar(min = 0, max = file_count, initial = 0,
                                 width = 80, style = 3)
  for(i in 1:file_count) {
    file_name <- sub(".*/([0-9:]{4})/", "", data[i])
    file_url <- RCurl::getBinaryURL(data[i], userpwd = userpwd)
    writeBin(file_url, file_name)
    setTxtProgressBar(progress_bar, i)
  }
  close(progress_bar)
  if(!is.null(dir)) setwd(old_dir)
}
