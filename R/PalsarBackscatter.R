#' Palsar Mosaic to backscatter
#' 
#' Converts ALOS PALSAR global mosaic digital numbers (DN) to backscatter 
#' 
#'@param palsar, a raster object (layer,stack,or brick) of ALOS PALSAR mosaic
#'@return A corresponding raster object with the DN converted to backscatter

PalsarBackscatter  <- function(palsar){
  
   backscatter<- 10*log10((palsar+0.001)^2) +-83
  
return(backscatter)

# End of script
}