#' Time raster
#'
#' @export
#' @param x one of \code{\link[raster]{RasterStack-class}} or
#' \code{\link[raster]{RasterBrick-class}}
#' @param dates An object with dates, one of object of class
#' \code{\link[xts]{xts}}, or a \code{character} or \code{Date} class
#' vector of dates. If \code{character}, coerced internally to \code{Date}
#'
#' @return An object of class \code{\link{TimeRaster-class}}
#'
#' @examples \dontrun{
#' zip <- system.file("examples", "prismrain.zip", package = "timeraster")
#' dir <- paste0(tempdir(), "/prismrain")
#' dir <- "prismrain"
#' dir.create(dir)
#' unzip(zip, exdir = dir)
#' files <- list.files(dir, full.names = TRUE, pattern = ".tif$", all.files = TRUE)
#'
#' # Create raster stack from file paths
#' rf <- raster::stack(files)
#'
#' # Create a time series object
#' library("xts")
#' ts <- xts(1:365, as.Date("2014-01-01") + 0:364)
#'
#' # Create TimeRaster object
#' ## Using new()
#' rftr <- new("TimeRaster", rf, ts = ts)
#'
#' ## Use convenience function time_raster()
#' ### With list of files as input - & xts object
#' rftr <- time_raster(files, ts)
#' ### With RasterStack class as input - & xts object
#' rftr <- time_raster(rf, ts)
#' ### With RasterStack class as input - & Date class object
#' rftr <- time_raster(rf, as.Date("2014-01-01") + 0:364)
#'
#' # plot data
#' plot(rftr[["2014-10-01 TO 2014-10-03"]])
#' plot(rftr[["UPTO MONTHS"]])
#' }
time_raster <- function(x, dates) {
  if (!is(x, "RasterStack") && !is(x, "RasterBrick")) {
    x <- raster::stack(x)
  }
  if (!("xts" %in% class(dates))) {
    dates <- xts::xts(1:length(dates), dates)
  }
  #override content to be 1..n
  dates[] <- 1:length(dates)
  #midnight sometimes is previous day so put midday but still rounds down
  zoo::index(dates) <- trunc(zoo::index(dates)) + 0.4
  return(new("TimeRaster", x, ts = dates))
}
