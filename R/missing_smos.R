#' Print the dates for which BEC-SMOS soil moisture data were not found
#'
#' This function prints out the dates for which BEC-SMOS soil moisture data with
#' specified \code{frequency} and \code{orbit} arguments were not  found on the
#' BEC server. This information is automatically generated while running
#' \code{\link[=find_smos]{find_smos()}}, but displayed only if requested by the
#' user.
#'
#' @references Pablos M, Gonzalez-Haro C, Portal G, Piles M, Vall-llossera M,
#' Portabella M (2022). SMOS L4 Surface Soil Moisture downscaled maps at 1 km
#' EASE-2 (reprocessed mode) (V.6.0) [Dataset].
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
