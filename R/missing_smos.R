#' Print the dates for which BEC-SMOS soil moisture data have not been found
#'
#' This function prints out the dates for which BEC-SMOS soil moisture data with
#' specified \code{frequency} and \code{orbit} arguments have not been found on
#' the BEC server. This information is automatically generated while running
#' \code{\link[=find_smos]{find_smos()}}, but displayed only if requested by the
#' user.
#'
#' @return a character string containing dates for which the data files were not
#' found on the server.
#'
#' @examples
#' \dontrun{
#' missing_smos()
#' }
#'
#' @export

missing_smos <- function() {
  if(Sys.getenv("missing") == "") {
    warning(simpleWarning(paste("No SMOS data have been processed yet.",
                                "Run 'find_smos()' before checking for",
                                "missing data.")))
  } else print(Sys.getenv("missing"))
}
