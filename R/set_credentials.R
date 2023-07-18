#' Set credentials to access Barcelona Expert Center (BEC) server
#'
#' To use some functionalities of \code{smosr} package (e.g. access the server
#' or download data to a local computer), the user should first register at
#' Barcelona Expert Center (BEC) webpage. This function allows the authenticated
#' users to set their BEC credentials (username and password) for the current R
#' session which are used internally in \code{\link[=find_smos]{find_smos()}}
#' and \code{\link[=download_smos]{download_smos()}}.
#'
#' If you do not have your BEC login details yet, please register on
#' https://bec.icm.csic.es/bec-ftp-service-registration/.
#'
#' @param username a character string containing BEC server username.
#'
#' @param password a character string containing BEC server password.
#'
#' @return a character string with the inputs pasted together in the format
#' required by \code{\link[=find_smos]{find_smos()}} and
#' \code{\link[=download_smos]{download_smos()}}.
#'
#' @examples
#' \dontrun{
#' set_credentials("username", "password")
#' }
#'
#' @export

set_credentials <- function(username, password) {
  userpass <- paste0(username, ":", password)
  Sys.setenv(userpass = userpass)
}
