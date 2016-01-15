#' Convenience function to make a RasterStack
#'
#' @export
#' @param x Vector or list of file paths
#' @param ... Further args passed on to \code{\link[raster]{stack}}
#'
#' @return An object of \code{\link[raster]{RasterStack-class}}
#' @seealso \code{\link[raster]{stack}}
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
#' res <- make_raster(files)
#'
#' # NetCDF
#' x <- "/Users/sacmac/Downloads/lfndx_Ayear_PRISM_historical_v0_2014-2014.nc"
#' res <- make_raster(x)
#' }
make_raster <- function(x, ...) {
  raster::stack(x, ...)
}
