#' Time raster
#'
#' @export
#' @param rast An object of class \code{RasterStack} or \code{RasterBrick}
#' @param datelist A date list
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
time_raster <- function(rast, datelist) {
  if (!is(rast, "RasterStack") && !is(rast, "RasterBrick")) {
    rast <- raster::stack(rast)
  }
  if (!("xts" %in% class(datelist))) {
    datelist <- xts::xts(1:length(datelist), datelist)
  }
  #override content to be 1..n
  datelist[] <- 1:length(datelist)
  #midnight sometimes is previous day so put midday but still rounds down
  zoo::index(datelist) <- trunc(zoo::index(datelist)) + 0.4
  return(new("TimeRaster", rast, ts = datelist))
}
