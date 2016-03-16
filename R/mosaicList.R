#' Generate raster mosaic from a list of files 
#' 
#' @description
#' Mosaics multiple raster files into a single object.
#' This is intended for layers that do not require adjustments such as DEM files. 
#' 
#' @param rasList a list of raster objects
#' 
#' @author
#' Tom Higginbottom
#' 
#' @examples
#' #Set working directory to folder containing DTM files
#' setwd("D:/Projects/topography/Lidar_DTM_tiles/")
#' list all DTM files present, recursive= T to ensure sub folders are examined
#' dtm.list <- list.files(pattern=glob2rx("*.asc"), full.names=TRUE,recursive=TRUE)
#' mosaics <- mosaicList(rasList = dtm.list)
#'
#' @return a mosaiced raster object


mosaicList <- function(rasList){
  
#Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  raster.list$fun <- mean
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}
  
  