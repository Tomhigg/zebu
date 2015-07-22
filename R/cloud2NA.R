#' Cloud To NA
#' 
#' Function to mask out all areas classed as cloud/cloud shadow/water/adjacent cloud by the CFmask layer provided by CDR
#' Landsat imagery. 
#' 
#' @param x rastwe layer to be masked, ie the original
#' @param mask raster layer used as the mask, all pixels not 0 will be NA, potentially use reclass to 
#' 
#' @return a multiband tiff image where non 0 pixels have been removed (NA'D)

cloud2NA <- function(x, mask){
  x[y != 0] <- NA
  return(x)
  }
