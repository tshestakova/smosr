#' Draw a raster image of BEC-SMOS soil moisture data
#'
#' This function draws a raster image of BEC-SMOS soil moisture data
#' corresponding to a single data file and specific geographical extent.
#'
#' This function reads an original BEC-SMOS soil moisture data file in NetCDF
#' format, converts data from EASE-2 grid cells to geographic coordinates, and
#' draws a raster image of soil moisture estimates in ‘latlon’ projection. The
#' image can be drawn for a specific geographical extent if requested by the
#' user. Otherwise, the entire dataset across Europe (between 28 and 72 degrees
#' north and -11 and 40 degrees east) will be plotted. Note that due to high
#' resolution of the data (~1 km), the execution of this function may take a
#' long time to be completed depending on the amount of data to be drawn.
#'
#' **** Quality assurance (QA) ****
#'
#' QA flags are coded by four significant bits as described below:
#'
#' \tabular{rcl}{
#' \tab -------------- \tab -------------------------------------------------------------------------------- \cr
#' \tab Bit position \tab Bit value \cr
#' \tab -------------- \tab -------------------------------------------------------------------------------- \cr
#' \tab [0] \tab 0 - Brightness temperature not affected by sea-land contamination \cr
#' \tab     \tab 1 - Brightness temperature corrected by sea-land contamination \cr
#' \tab ------------- \tab -------------------------------------------------------------------------------- \cr
#' \tab [1] \tab 0 - Radio Frequency Interference (RFI) not flagged in ESA L1C \cr
#' \tab     \tab brightness temperature \cr
#' \tab     \tab 1 - RFI flagged in ESA L1C brightness temperature \cr
#' \tab ------------- \tab -------------------------------------------------------------------------------- \cr
#' \tab [2] \tab 0 - L3 soil moisture with data obtained from L2 retrievals \cr
#' \tab     \tab 1 - L3 soil moisture with data obtained from a linear model \cr
#' \tab ------------- \tab -------------------------------------------------------------------------------- \cr
#' \tab [3] \tab 0 - L4 soil moisture values within the interval [0,1] m^3/m^3 \cr
#' \tab     \tab 1 - L4 soil moisture values outside the interval [0,1] m^3/m^3 \cr
#' \tab ------------- \tab -------------------------------------------------------------------------------- \cr
#' }
#'
#' In case of the 3-day averaged data, each bit of the quality flag is activated
#' if at least one soil moisture estimate during the corresponding time interval
#' is affected.
#'
#' @references Pablos M, Gonzalez-Haro C, Portal G, Piles M, Vall-llossera M,
#' Portabella M (2022). SMOS L4 Surface Soil Moisture downscaled maps at 1 km
#' EASE-2 (reprocessed mode) (V.6.0) [Dataset].
#'
#' @param data a character string containing a link to a single BEC-SMOS data
#' file stored on the local computer.
#'
#' @param lat a numeric vector of length 2 containing latitudinal bounds of
#' the plotting region (in ‘latlon’ projection). Default value is \code{NULL}
#' meaning that all data between min and max latitudes are drawn.
#'
#' @param lon a numeric vector of length 2 containing longitudinal bounds of
#' the plotting region (in ‘latlon’ projection). Default value is \code{NULL}
#' meaning that all data between min and max longitudes are drawn.
#'
#' @param QA a numeric vector specifying the desired data quality to be plotted.
#' Possible values range from 0 (good quality data) to 15. To know the meanings
#' of QA flags, see Details.
#'
#' @return a raster image
#'
#' @examples
#' \dontrun{
#' # to draw a raster image of soil moisture data corresponding to the first BEC-SMOS file
#' # from a list produced by list_smos() and within the specified geographical bounds#'
#' smos_files <- list_smos()
#' lat <- c(35.00, 45.00)
#' lon <- c(-10.50, 4.50)
#' plot_raster_smos(data = smos_files[1], lat = lat, lon = lon)
#' }
#'
#' @importFrom fields image.plot
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom grDevices dev.list
#' @importFrom grDevices dev.off
#' @importFrom grDevices hcl.colors
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 ncvar_get
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra lines
#' @importFrom terra vect
#'
#' @export

plot_raster_smos <- function(data, lat = NULL, lon = NULL, QA = NULL) {
  nc_data <- ncdf4::nc_open(data)
  nc_lat <- ncdf4::ncvar_get(nc_data, "lat")
  nc_lon <- ncdf4::ncvar_get(nc_data, "lon")
  nc_time <- ncdf4::ncvar_get(nc_data, "time")
  nc_date <- as.Date(as.POSIXct(nc_time, origin = "1970-01-01"))
  nc_sm <- ncdf4::ncvar_get(nc_data, "SM")
  sm_fillvalue <- ncdf4::ncatt_get(nc_data, "SM", "_FillValue")
  nc_sm[nc_sm == sm_fillvalue$value] <- NA
  nc_qa <- ncdf4::ncvar_get(nc_data, "quality_flag")
  qa_fillvalue <- ncdf4::ncatt_get(nc_data, "quality_flag", "_FillValue")
  nc_qa[nc_qa == qa_fillvalue$value] <- NA
  ncdf4::nc_close(nc_data)
  smos_coastline <- terra::vect(system.file("smos_coastline.shp",
                                            package = "smosr"))
  if(is.null(lat)) {
    plot_lat <- c(min(nc_lat), max(nc_lat))
    lat_grid <- nc_lat
  } else {
      if(length(lat) != 2)
        stop(simpleError(paste("Argument 'lat' must be of length 2. Modify the",
                               "plotting parameters accordingly to proceed.")))
      plot_lat <- lat
      lat_grid <- nc_lat[nc_lat >= min(plot_lat) & nc_lat <= max(plot_lat)]
      if(length(lat_grid) == 0)
        stop(simpleError(paste("Argument 'lat' should be between 28\u00B0N and",
                               "72\u00B0N. Modify the plotting parameters",
                               "accordingly to proceed.")))
      nc_sm <- nc_sm[, nc_lat >= min(plot_lat) & nc_lat <= max(plot_lat)]
      nc_qa <- nc_qa[, nc_lat >= min(plot_lat) & nc_lat <= max(plot_lat)]
  }
  if(is.null(lon)) {
    plot_lon <- c(min(nc_lon), max(nc_lon))
    lon_grid <- nc_lon
  } else {
      if(length(lon) != 2)
        stop(simpleError(paste("Argument 'lon' must be of length 2. Modify the",
                               "plotting parameters accordingly to proceed.")))
      plot_lon <- lon
      lon_grid <- nc_lon[nc_lon >= min(plot_lon) & nc_lon <= max(plot_lon)]
      if(length(lon_grid) == 0)
        stop(simpleError(paste("Argument 'lon' should be between -11\u00B0E",
                               "and 40\u00B0E. Modify the plotting parameters",
                               "accordingly to proceed.")))
      nc_sm <- nc_sm[nc_lon >= min(lon_grid) & nc_lon <= max(lon_grid), ]
      nc_qa <- nc_qa[nc_lon >= min(lon_grid) & nc_lon <= max(lon_grid), ]
  }
  if(!is.null(QA)) nc_sm[!nc_qa %in% QA] <- NA
  if(all(is.na(nc_sm)))
    stop(simpleError(paste("Nothing to display: no data of required quality",
                           "(QA) in the specified area of interest (lat, lon)",
                           "were found.")))
  if(!is.null(lat) || !is.null(lon)) {
    crop_extent <- terra::ext(plot_lon[1], plot_lon[2], plot_lat[1], plot_lat[2])
    smos_coastline <- terra::crop(smos_coastline, crop_extent)
  }
  file_name <- sub(".*/", "", data)
  freq <- "1d"
  match <- grep(freq, file_name, value = TRUE)
  if(length(match) == 0) freq <- "3d"
  orbit <- "A"
  match <- grep(orbit, file_name, value = TRUE)
  if(length(match) == 0) orbit <- "D"
  if(freq == "1d") {
    title <- "Daily BEC-SMOS product"
  } else title <- "3-day BEC-SMOS product"
  if(is.null(QA)) QA <- "none"
  if(orbit == "A") {
    subtitle <- paste(c(as.character(nc_date), "/ Ascending pass ( QA =", QA,
                        ")"), collapse = " ")
  } else subtitle <- paste(c(as.character(nc_date), "/ Descending pass ( QA =",
                             QA, ")"), collapse = " ")
  old_parms <- graphics::par(no.readonly = TRUE)
  on.exit(par(old_parms))
  while(!is.null(grDevices::dev.list())) grDevices::dev.off()
  legend_title <- expression("Soil moisture (" * m^3 / m^3 * ")")
  sm_min <- round(min(nc_sm, na.rm = TRUE) - 0.05, 1)
  sm_max <- round(max(nc_sm, na.rm = TRUE) + 0.05, 1)
  color_breaks <- seq(sm_min, sm_max, 0.1)
  color_palette <- grDevices::hcl.colors(length(color_breaks)-1, "RdYlBu")
  fields::image.plot(lon_grid, lat_grid, nc_sm, xlim = plot_lon, ylim = plot_lat,
                     main = title, col = color_palette, breaks = color_breaks,
                     xlab = "Latitude (\u00B0N)", ylab = "Longitude (\u00B0E)",
                     legend.args = list(text = legend_title, side = 4, line = -2,
                     cex = 0.9), axis.args = list(cex.axis = 0.8), axes = FALSE)
  graphics::axis(1, las = 1, cex.axis = 0.9)
  graphics::axis(2, las = 3, cex.axis = 0.9)
  graphics::box()
  graphics::mtext(text = subtitle, side = 3, line = 0.35, cex = 1.0)
  terra::lines(smos_coastline, col = "black")
}
