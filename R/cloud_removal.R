#' Remove Clouds in Landsat Imagery
#' 
#' Function to mask out all areas classed as cloud/cloud shadow/water/adjacent cloud by the CFmask layer provided by CDR
#' Landsat imagery. This is the final stage of landsat preprocessing workflow. (espa_extract, auto_preprocess_landsat).
#' SR and mask files should be in the same folder. 
#' 
#' @param path path to a multiband tiff image produced by auto_preprocess_landsat in the team_lucc package
#' @param outfolder folder where the output image will be saved, file name will be the same as input
#' 
#' @return a multiband tiff image where non "clear" pixels have been removed (NA'D)




cloud_removal  <- function(path,outfolder){
      
  #load SR tiff as brick
  sr_stack<-  brick(path)
  
  #create path for cfmask file, 
 raw.path  <- file_path_sans_ext(path)
 mask_path  <- paste0(raw.path, "_masks.tif")
    
  #load cfmask and extract from brick
  cfmask  <- brick(mask_path)
  fmask <- cfmask[[2]]
  
    #make name for output file 
  sr_basename  <- basename(path)
  sr_outname <- paste0(outfolder,sr_basename)
  
  
  sr_stack_masked <- overlay(x = sr_stack, y = fmask, fun = cloud2NA,filename= sr_outname )
      
}



