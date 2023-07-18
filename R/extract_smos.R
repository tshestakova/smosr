#' Extract BEC-SMOS soil moisture estimates for specific geographical locations
#'
#' This function facilitates reading the original BEC-SMOS soil moisture data
#' files and extracting relevant information for specific geographical locations
#' by using Lat/Lon coordinates in decimal degrees.
#'
#' This function reads the original BEC-SMOS soil moisture data files in NetCDF
#' format, converts data from EASE-2 grid cells to geographic coordinates, and
#' extracts relevant information for Lat/Lon locations specified by the user.
#'
#' The data retrieved from each data file includes:
#'
#' - the coordinates of spatial points (Lon and Lat) from which the data were
#' extracted;
#'
#' - frequency and SMOS orbit of each file over which the function iterated;
#'
#' - date and time when the data was obtained;
#'
#' - soil moisture estimate (SM);
#'
#' - quality assurance (QA) flag corresponding to each SM estimate.
#'
#' The output of this function could be saved as a CSV file.
#'
#' @references Pablos M, Gonzalez-Haro C, Portal G, Piles M, Vall-llossera M,
#' Portabella M (2022). SMOS L4 Surface Soil Moisture downscaled maps at 1 km
#' EASE-2 (reprocessed mode) (V.6.0) [Dataset].
#'
#' @param data a character vector as produced by \code{list_smos()} containing
#' links to the data files on the local computer.
#'
#' @param lat a numeric vector containing latitudes of points to extract the
#' data from (in ‘latlon’ projection).
#'
#' @param lon a numeric vector containing longitudes of points to extract the
#' data from (in ‘latlon’ projection).
#'
#' @param save a logical vector indicating whether the output should  be saved
#' as a CSV file. Default is \code{FALSE}.
#'
#' @param filename a character string naming a file for saving the output. If
#' \code{save} = \code{TRUE} and no \code{filename} is specified by the user,
#' the data is saved in a file with a generic name ‘SM_output.csv’.
#'
#' @return a data.matrix with the relevant information as described in Details.
#'
#' @examples
#' \dontrun{
#' # to iterate over BEC-SMOS data files stored in the current working directory
#' # and extract soil moisture estimates for the specified geographical locations
#' smos_files <- list_smos()
#' lat <- c(40.42, 41.90, 48.86, 52.50, 59.91)
#' lon <- c(-3.70, 12.50, 2.35, 13.40, 10.75)
#' sm_estimates <- extract_smos(data = smos_files, lat = lat, lon = lon)
#' }
#'
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 ncvar_get
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils write.csv
#'
#' @export

extract_smos <- function(data, lat, lon, save = FALSE,
                         filename = "SM_output.csv") {
  poi <- set_coords(lat, lon)
  poi_count <- nrow(poi)
  file_count <- length(data)
  sm_data <- matrix(nrow = file_count * poi_count, ncol = 8)
  colnames(sm_data) <- c("Lat", "Lon", "Freq", "Orbit",
                         "Date", "Time", "SM", "QA")
  progress_bar <- utils::txtProgressBar(min = 0, max = file_count, initial = 0,
                                 width = 80, style = 3)
  for(i in 1:file_count) {
    file_name <- sub(".*/", "", data[i])
    freq <- sub(".*([13]d).*", "\\1", file_name)
    if(!freq %in% c("1d", "3d")) freq <- NA
    orbit <- sub(".*([AD]).*", "\\1", file_name)
    if(!orbit %in% c("A", "D")) orbit <- NA
    nc_data <- ncdf4::nc_open(data[i])
    nc_lat <- ncdf4::ncvar_get(nc_data, "lat", verbose = F)
    nc_lon <- ncdf4::ncvar_get(nc_data, "lon", verbose = F)
    nc_time <- ncdf4::ncvar_get(nc_data, "time")
    nc_date <- as.Date(as.POSIXct(nc_time, origin = "1970-01-01"))
    nc_hour <- format(as.POSIXct(nc_time, origin = "1970-01-01"),
                      format = "%H:%M:%S")
    nc_sm <- ncdf4::ncvar_get(nc_data, "SM")
    sm_fillvalue <- ncdf4::ncatt_get(nc_data, "SM", "_FillValue")
    nc_sm[nc_sm == sm_fillvalue$value] <- NA
    nc_qa <- ncdf4::ncvar_get(nc_data, "quality_flag")
    qa_fillvalue <- ncdf4::ncatt_get(nc_data, "quality_flag", "_FillValue")
    nc_qa[nc_qa == qa_fillvalue$value] <- NA
    ncdf4::nc_close(nc_data)
    for(k in 1:poi_count) {
      n <- poi_count * (i - 1) + k
      lat_nearest <- which.min(abs(nc_lat - poi[k,1]))
      lon_nearest <- which.min(abs(nc_lon - poi[k,2]))
      sm_data[n,1] <- poi[k,1]
      sm_data[n,2] <- poi[k,2]
      sm_data[n,3] <- freq
      sm_data[n,4] <- orbit
      sm_data[n,5] <- as.character(nc_date)
      sm_data[n,6] <- as.character(nc_hour)
      sm_data[n,7] <- nc_sm[lon_nearest, lat_nearest]
      sm_data[n,8] <- nc_qa[lon_nearest, lat_nearest]
    }
    utils::setTxtProgressBar(progress_bar, i)
  }
  close(progress_bar)
  if(save) utils::write.csv(sm_data, filename)
  return(sm_data)
}

set_coords <- function(lat, lon) {
  if(length(lon) != length(lat))
    stop(simpleError(paste("Lat and lon vectors are unequal. Make sure they",
                            "are of the same length to avoid this error.")))
  lat_exceed <- which(lat < 28 | lat > 72)
  lon_exceed <- which(lon < -11 | lon > 40)
  coords_exceed <- sort(unique(append(lon_exceed, lat_exceed)))

  if(length(coords_exceed) != 0) {
    lat <- lat[c(-coords_exceed)]
    lon <- lon[c(-coords_exceed)]
    if(length(lat) == 0 || length(lon) == 0) {
      stop(simpleError(paste("No data can be retrieved because the",
                             "submitted coordinates are out of allowed",
                             "bounds (lat: 28\u00B0N to 72\u00B0N,",
                             "lon: -11\u00B0E to 40\u00B0E). Provide",
                             "valid coordinates to proceed.")))
    } else
        warning(simpleWarning(paste("Some submitted coordinates are out of",
                                    "allowed bounds and are discarded. Note",
                                    "that valid coordinates should be between",
                                    "28\u00B0 and 72\u00B0N and -11\u00B0 and",
                                    "40\u00B0E.")))
  }
  lat_dup <- duplicated(lat); lat_dup
  lon_dup <- duplicated(lon); lon_dup
  lat <- lat[!(lat_dup & lon_dup)]
  lon <- lon[!(lat_dup & lon_dup)]
  poi <- cbind(lat, lon)
  return(poi)
}

