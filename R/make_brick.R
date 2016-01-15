#' Convenience function to make a RasterBrick
#'
#' @export
#' @param x A file path, or a URL to a file
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
#' res <- make_brick(files[1])
#'
#' # native if TRUE uses raster package, if FALSE uses rgdal
#' make_brick(files[1], native = FALSE)
#' make_brick(files[1], native = TRUE)
#'
#' # from a NetCDF file
#' ## dates available
#' url <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.2016.v2.nc"
#' tf <- tempfile(fileext = ".nc")
#' download.file(url, tf, mode = 'wb')
#' x <- make_brick(tf)
#' time_raster(x)
#'
#' nc <- system.file("examples", "prismrain.nc", package = "timeraster")
#' make_brick(nc)
#' }
make_brick <- function(x, ...) {
  if (length(x) > 1) stop("Can only pass 1 file or URL to make_brick", call. = FALSE)
  tmp <- raster::brick(x, ...)
  if (!is.null(names(tmp))) attr(tmp, "dates") <-
    gsub("\\.", "-", gsub("[[:alpha:]]", "", names(tmp)))
  return(tmp)
}
