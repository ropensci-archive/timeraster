#' Get time series
#'
#' @export
#' @param x An object of class \code{\link{TimeRaster-class}}
#' @param long (numeric) a longitude value
#' @param lat (numeric) a latitude value
#'
#' @return An \code{xts} object
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
#' res <- time_raster(files, ts)
#'
#' # Pull out xts object
#' get_ts(res, -67, 45)
#' }
get_ts <- function(x, long, lat) {
  UseMethod("get_ts")
}

#' @rdname get_ts
get_ts.TimeRaster <- function(x, long, lat) {
  getTS(x, long, lat)
}

#' @rdname get_ts
get_ts.default <- function(x, long, lat) {
  stop(sprintf("get_ts method not implemented for %s.", class(x)[1]))
}
