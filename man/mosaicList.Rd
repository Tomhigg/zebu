% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mosaicList.R
\name{mosaicList}
\alias{mosaicList}
\title{Generate raster mosaic from a list of files}
\usage{
mosaicList(rasList)
}
\arguments{
\item{rasList}{a list of raster objects}
}
\value{
a mosaiced raster object
}
\description{
Mosaics multiple raster files into a single object.
This is intended for layers that do not require adjustments such as DEM files.
}
\examples{
#Set working directory to folder containing DTM files
setwd("D:/Projects/topography/Lidar_DTM_tiles/")
list all DTM files present, recursive= T to ensure sub folders are examined
dtm.list <- list.files(pattern=glob2rx("*.asc"), full.names=TRUE,recursive=TRUE)
mosaics <- mosaicList(rasList = dtm.list)

}
\author{
Tom Higginbottom
}

