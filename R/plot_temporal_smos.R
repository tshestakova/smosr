#' Plot temporal series of BEC-SMOS soil moisture data
#'
#' This function plots temporal series of BEC-SMOS soil moisture data
#' extracted for specific geographical locations using \code{extract_smos()}.
#'
#' Note that the data characterized by the same frequency and SMOS orbit can
#' be drawn at a time. If the dataset to plot contains a mixture of temporal
#' resolution and/or SMOS passes, arguments \code{frequency} and \code{orbit}
#' must be specified.
#'
#' @param data a data.matrix as produced by \code{list_smos()} containing soil
#' moisture data to plot.
#'
#' @param freq an integer specifying temporal frequency of the data. Possible
#' values are: 1 - for daily data, 3 - for 3-day moving averages, and NULL -
#' for cases when data frequency is irrelevant. Default value is \code{NULL}.
#'
#' @param orbit a character (or character string) specifying SMOS orbit
#' corresponding to the data. Possible values are: ‘a’, ‘asc’, and ‘ascending’ -
#' for an ascending pass, or ‘d’, ‘des’, or ‘descending’ - for a descending
#' pass, and NULL - for cases when orbit is irrelevant. Default value is
#' \code{NULL}.
#'
#' @param dates a object of class \code{Date} or a character string formatted
#' as ‘yyyy-mm-dd’ (e.g. ‘2010-06-01’) which specifies the dates to plot the
#' data for.
#'
#' @param QA a numeric vector specifying the desired quality assurance of the
#' data to be plotted. Possible values range from 0 (good quality data) to 15.
#' To know the meanings of QA > 0, please refer to the technical note on the
#' BEC-SMOS soil moisture products available at
#' \url{https://digital.csic.es/handle/10261/303808}.
#'
#' @examples
#' \dontrun{
#' # to plot extracted temporal series of soil moisture data
#' # with the specified frequency, SMOS orbit and QA
#' smos_data <- list_smos()
#' lat <- c(40.42, 41.90, 48.86, 52.50, 59.91)
#' lon <- c(-3.70, 12.50, 2.35, 13.40, 10.75)
#' sm_estimates <- extract_smos(data = smos_data, lat = lat, lon = lon)
#' plot_temporal_smos(data = sm_estimates, freq = 3, orbit = "d", QA = 0)
#' }
#'
#' @importFrom graphics par
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices dev.size
#'
#' @export

plot_temporal_smos <- function(data, freq = NULL, orbit = NULL,
                               dates = NULL, QA = NULL) {
  data_freq <- sort(unique(data[,3]))
  if(!is.null(freq)) freq <- check_freq(freq)
  if(is.null(freq) && length(data_freq) > 1) {
    stop(simpleError(paste("You tried to plot data obtained at different",
                           "temporal frequencies. Specify 'freq' argument",
                           "to avoid this error.")))
  } else {
      if(length(data_freq) > 1) {
        data[data[,3] != freq, 7] <- NA
      } else {
          if(!is.null(freq) && data_freq != freq)
            stop(simpleError(paste("Data for the specified frequency was",
                                   "not found. Submit another dataset or",
                                   "modify plotting parameters to avoid",
                                   "this error.")))
    }
  }
  data_orbit <- sort(unique(data[,4]))
  if(!is.null(orbit)) freq <- check_orbit(orbit)
  if(is.null(orbit) && length(data_orbit) > 1) {
    stop(simpleError(paste("You tried to plot data obtained for",
                            "different SMOS passes. Specify 'orbit'",
                            "argument to avoid this error.")))
  } else {
    if(length(data_orbit) > 1) {
      data[data[,4] == orbit, 7] <- NA
    } else {
      if(!is.null(orbit) && data_orbit != orbit)
        stop(simpleError(paste("Data for the specified orbit was not found.",
                               "Submit another dataset or modify plotting",
                               "parameters to avoid this error.")))
    }
  }
  if(!is.null(dates)) {
    if(class(dates) != "Date") dates <- convert_dates(dates)
    data[!as.Date(data[,5], "%Y-%m-%d") %in% dates, 7] <- NA
  }
  if(!is.null(QA)) {
    if(all(QA < 0) || all(QA > 15))
      stop(simpleError(paste("Argument 'QA' is invalid. It can",
                             "take values between 0 and 15.")))
    data[!data[,8] %in% QA, 7] <- NA
  }
  smos_lat <- as.numeric(data[,1])
  smos_lon <- as.numeric(data[,2])
  smos_coords <- paste0(format(round(smos_lat, 3), nsmall = 3), ", ",
                        format(round(smos_lon, 3), nsmall = 3))
  poi_count <- length(unique(smos_coords))
  smos_dates <- as.Date(data[,5], "%Y-%m-%d")
  smos_sm <- as.numeric(data[,7])
  if(all(is.na(smos_sm)))
    stop(simpleError(paste("No data to plot. Modify plotting",
                           "parameters to proceed.")))
  smos_plot <- data.frame(smos_coords, smos_dates, smos_sm)
  smos_plot <- unique(smos_plot)
  smos_plot <- tidyr::pivot_wider(smos_plot, names_from = "smos_coords",
                                  values_from = "smos_sm")
  smos_plot <- as.data.frame(smos_plot)
  smos_plot <- smos_plot[order(as.Date(smos_plot[,1], "%Y-%m-%d")), ]
  smos_plot <- smos_plot[, colSums(is.na(smos_plot)) < nrow(smos_plot)]
  smos_legend <- colnames(smos_plot[c(-1)])
  poi_count <- ncol(smos_plot) - 1
  if(poi_count == 0)
    stop(simpleError(paste("No data to plot. Modify the plotting",
                           "arguments to proceed.")))
  old_parms <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(5.7, 3.2, 1.5, 6.8), mgp = c(1.9, 0.5, 0), tcl = -0.25)
  y_lable <- expression("Soil moisture (" * m^3 / m^3 * ")")
  from_min <- round(min(smos_plot[,c(-1)], na.rm = TRUE) - 0.05, 1)
  to_max <- round(max(smos_plot[,c(-1)], na.rm = TRUE) + 0.05, 1)
  plot(smos_plot[,1], smos_plot[,2], type = "n", ylim = c(from_min, to_max),
       axes = FALSE, ylab = y_lable, xlab = "", cex = 1,
       main = "BEC-SMOS soil moisture", font.main = 2, cex.main = 1.2)
  graphics::title(xlab = "Date", line = 4.5)
  symb_plot <- line_plot <- c()
  j = 1
  for(i in 1:poi_count) {
    symb_plot <- c(symb_plot, 20 + j)
    line_plot <- c(line_plot, j)
    if((i/8) %% 1 == 0) j <- j + 1
  }
  for (i in 1:poi_count) {
    graphics::lines(smos_plot[,1], smos_plot[,i+1],
                    lty = line_plot[i], col = i, lwd = 1.5)
    graphics::points(smos_plot[,1], smos_plot[,i+1],
                     pch = symb_plot[i], col = i, cex = 0.9,
                     bg = adjustcolor(i, alpha.f = 0.5), lwd = 1.5)
  }
  legend_key <- round((graphics::par("pin")[2] - 0.27) / 0.16 - 1, 0)
  if(length(smos_legend) < legend_key + 1) {
    legend_trunc <- smos_legend
  } else {
      if(legend_key < 3) legend_key <- 3
      legend_trunc <- smos_legend[1:legend_key]
      legend_trunc <- c(legend_trunc, "...")
  }
  poi_count <- length(legend_trunc)
  y_breaks <- seq(from = from_min, to = to_max, by = 0.1)
  graphics::axis(2, at = y_breaks, las = 2, cex.axis = 0.9)
  graphics::axis(1, smos_plot[,1], format(smos_plot[,1], "%Y-%m-%d"),
                 cex.axis = 0.9, las = 2)
  graphics::box()
  graphics::legend("topleft", legend_trunc, title = "LEGEND\n(lat, lon)",
                   lty = line_plot, col = seq(1:poi_count), pch = symb_plot,
                   pt.bg = adjustcolor(seq(1:poi_count), alpha.f = 0.5),
                   bty = "n", inset = c(1,0), xpd = TRUE, cex = 0.8,
                   lwd = 1.5, seg.len = 1.5)
  graphics::par(old_parms)
}
