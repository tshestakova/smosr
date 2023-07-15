
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smosr

## Overview

The goal of smosr is to automate accessing, downloading and importing
ESA-BEC Soil Moisture and Ocean Salinity (SMOS) data into R.
Particularly, it includes functions to search for, acquire, extract, and
plot BEC-SMOS L4 soil moisture data downscaled to ~1 km spatial
resolution (EASE-grid v.2).

## Installation

Install the major version of smosr from CRAN repository with:

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("smosr")
```

### Development version

To get a bug fix or to use new features of the package, you can install
the development version of smosr from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("tshestakova/smosr")
```

## Usage

``` r
library(smosr)
# set_credentials("username", "password")
start_date <- as.Date("2010-07-15")
end_date <- as.Date("2022-07-15")
date_range <- seq(start_date, end_date, by = 365)
smos_data <- find_smos(freq = 3, orbit = "des", dates = date_range)
#> Done. All requested files were successfully found.

dir.create("~/sm_example")
#> Warning in dir.create("~/sm_example"):
#> 'C:\Users\tshestakova\Documents\sm_example' already exists
download_smos(data = smos_data, dir = "~/sm_example")
smos_files <- list_smos(dir = "~/sm_example")
head(smos_files, 5)
#> [1] "C:/Users/tshestakova/Documents/sm_example/BEC_SM____SMOS__EUM_L4__D_20100715T183618_001km_3d_REP_v6.0.nc"
#> [2] "C:/Users/tshestakova/Documents/sm_example/BEC_SM____SMOS__EUM_L4__D_20110715T182802_001km_3d_REP_v6.0.nc"
#> [3] "C:/Users/tshestakova/Documents/sm_example/BEC_SM____SMOS__EUM_L4__D_20120714T182030_001km_3d_REP_v6.0.nc"
#> [4] "C:/Users/tshestakova/Documents/sm_example/BEC_SM____SMOS__EUM_L4__D_20130714T181231_001km_3d_REP_v6.0.nc"
#> [5] "C:/Users/tshestakova/Documents/sm_example/BEC_SM____SMOS__EUM_L4__D_20140714T180441_001km_3d_REP_v6.0.nc"

lat <- c(40.42, 41.90, 48.86, 52.50, 59.91)
lon <- c(-3.70, 12.50, 2.35, 13.40, 10.75)
sm_estimates <- extract_smos(data = smos_files, lat = lat, lon = lon)
head(sm_estimates, 12)
#>       Lat     Lon     Freq Orbit Date         Time       SM                     QA
#>  [1,] "40.42" "-3.7"  "3d" "D"   "2010-07-15" "20:36:18" NA                     NA
#>  [2,] "41.9"  "12.5"  "3d" "D"   "2010-07-15" "20:36:18" "0.0872999977946165"   "7"
#>  [3,] "48.86" "2.35"  "3d" "D"   "2010-07-15" "20:36:18" "0.206299994788424"    "2"
#>  [4,] "52.5"  "13.4"  "3d" "D"   "2010-07-15" "20:36:18" "0.191299995167356"    "6"
#>  [5,] "59.91" "10.75" "3d" "D"   "2010-07-15" "20:36:18" NA                     NA
#>  [6,] "40.42" "-3.7"  "3d" "D"   "2011-07-15" "20:28:02" NA                     NA
#>  [7,] "41.9"  "12.5"  "3d" "D"   "2011-07-15" "20:28:02" "0.025399999358342"    "7"
#>  [8,] "48.86" "2.35"  "3d" "D"   "2011-07-15" "20:28:02" "0.0235999994038139"   "6"
#>  [9,] "52.5"  "13.4"  "3d" "D"   "2011-07-15" "20:28:02" "0.0769999980548164"   "2"
#> [10,] "59.91" "10.75" "3d" "D"   "2011-07-15" "20:28:02" NA                     NA
#> [11,] "40.42" "-3.7"  "3d" "D"   "2012-07-14" "20:20:31" NA                     NA
#> [12,] "41.9"  "12.5"  "3d" "D"   "2012-07-14" "20:20:31" "0.000499999987368938" "7"
plot_temporal_smos(data = sm_estimates)
```

<img src="man/figures/README-example-temp.png" width="50%" />

``` r

lat <- c(35.00, 45.00)
lon <- c(-10.50, 4.50)
plot_raster_smos(data = smos_files[13], lat = lat, lon = lon, QA = c(0,1,2,3))
```

<img src="man/figures/README-example-sp.png" width="50%" />


## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/tshestakova/smosr/issues).

For questions and other discussion, please use [Stack
Overflow](https://stackoverflow.com/questions/) and the [RStudio
community](https://community.rstudio.com/).
