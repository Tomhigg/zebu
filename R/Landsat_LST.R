#' Landsat-based Land Surface Temperature
#' 
#Function to dervice Land surface temperature from landsat data, emmisivity calculated using NDVI as Sobrino et al (2004)



#'@param red, red waveband reflectance, Landsat4/5/7= band 3, Landsat 8=Band 4
#'@param nir near infared waveband reflectance, Landsat 4/5/7=band 4, Landsat 8=Band 5
#'@param bt Brightness temperature band. Landsat 4/5/7=Band 6, Landsat 8= bands 10 or 11
#'@param ex extent of processing 
#'@param  band thermal band used, Landsat 4/5/6= "TM", Landsat 8 "TIRS10" or "TIRS11" depending on band
#'@param output_prefix prefix for output file eg "Manchester_2003_july10" LST/ndvi.tif will be added for file 
#'
#'@return a stack containing LST and NDVI
#'@return writes NDVI and LST rasters to the current wd


LST  <- function(red,nir,bt,ex,fmask, band, output_prefix){

#1 Calculate NDVI
red = raster(red)*0.0001
red  <- crop(x = red,y = ex)

nir = raster(nir)*0.0001
nir  <- crop(x = nir, y = ex)

ndvi = (nir-red)/(nir+red)

#2 Calculate emissivity as per Sobrino et. al. (2004)
pv = (ndvi - 0.2)/(0.5 - 0.2)
em = (0.004*pv) + 0.986

#3 Import Landsat thermal band and resample to 30m
thermal = raster(bt)*0.1

b61=crop(x = thermal,y = ex)

#4 Compute temperature as per Weng et. al. (2004) and convert to Celsius
# Note: See Chander et. al. (2009) for applicable values.

wave  <- if (band == "TM") 11.45 else if (band == "TIRS10") 10.8  else 12

kelvin = b61/(1+ (wave*(b61/14380))*log(em))

#5 kelvin to celsius
celsius = kelvin-273


#5 mask out all clouds

fmask <- raster(fmask)
fmask  <- crop(x = fmask,y = ex)

m <- c(255,1,
       0,1,
       1,1,
       2,NA,
       3,NA,
       4,NA)
  
rclmat <- matrix(m, ncol=2, byrow=TRUE)
rc <- reclassify(fmask, rclmat)
celsius  <- mask(x = celsius,mask = rc)
ndvi  <- mask(x = ndvi,mask = rc)


#6 Write to file
celcius_name  <- paste0(output_prefix,"lst.tif")
ndvi_name  <- paste0(output_prefix,"ndvi.tif")

writeRaster(celsius, filename=celcius_name, format="GTiff")
writeRaster(ndvi, filename=ndvi_name, format="GTiff")

#stack and return

stack  <- stack(celsius,ndvi)
return(stack)

# End of script
}