#' Convenience function to make a RasterBrick
#'
#' @export
#' @param x Vector or list of file paths
#' @param ... Further args passed on to \code{\link[raster]{brick}}
#'
#' @return An object of \code{\link[raster]{RasterBrick-class}}
#' @seealso \code{\link[raster]{brick}}
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
#' make_brick(files[1])
#' }
make_brick <- function(x, ...) {
  raster::brick(x, ...)
}
