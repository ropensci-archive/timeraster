# make S3 object accessible to S4 objects
setOldClass("xts")

#' An S4 class to represent A Time Raster object
#'
#' @name TimeRaster-Class
#' @docType methods
#' @keywords methods
#' @slot ts An xts object
#' @param object an object
#' @param x xxxx
#' @param i xxxx
#' @param j xxxx
#' @param ... xxxx
#' @param stat xxxx
#' @param na.rm xxxx
#' @param asSample xxxx
setClass("TimeRaster",
         representation(ts = "xts"),
         contains = "RasterStack",
         validity = function(object) {
           if (nlayers(object) != length(object@ts))
             "Number of layers not equal length of timeseries"
           else
             TRUE
         }
)

#' @rdname TimeRaster-Class
setMethod(
  f = "show",
  signature = "TimeRaster",
  definition = function(object) {
    callNextMethod()
    cat("Time dimension has",toString(length(object@ts)),"time periods\n")
    show(head(index(object@ts)))

  }
)

#' @rdname TimeRaster-Class
setMethod(
  f = "[[",
  signature = "TimeRaster",
  definition = function(x,i,...) {
    if (is.numeric(i)) {return(callNextMethod(x,i,...))}
    if (is.character(i)) {
      args = parseit(i)
      return(doit(x,args))
    }# end if is.character #process
  } #end [[ function method
)

#' @rdname TimeRaster-Class
setGeneric("getTS",function(object,...) {
  standardGeneric("getTS")
})
setMethod(f = "getTS", signature = "TimeRaster",
          definition <- function(object,x,y) {
            row = rowFromY(object,y)
            col = colFromX(object,x)
            m = getValues(object,row)[col,]
            return(xts(as.vector(m),index(object@ts)))
          }
)

#' @rdname TimeRaster-Class
setMethod(
  f = "cellStats",
  signature = (x = "TimeRaster"),
  definition = function(x, stat = 'mean', na.rm = TRUE, asSample = TRUE, ...) {
    return(xts(callNextMethod(x, stat, na.rm, asSample), index(x@ts)))
  }
)
