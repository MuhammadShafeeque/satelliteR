% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcTopoCorr.R
\name{calcTopoCorr}
\alias{calcTopoCorr}
\alias{calcTopoCorr,Satellite-method}
\alias{calcTopoCorr,RasterStackBrick-method}
\alias{calcTopoCorr,RasterLayer-method}
\title{Correct for topographic effects.}
\usage{
\S4method{calcTopoCorr}{Satellite}(x, mask = TRUE)

\S4method{calcTopoCorr}{RasterStackBrick}(x, hillsh, cloudmask = NULL, ...)

\S4method{calcTopoCorr}{RasterLayer}(x, hillsh, cloudmask = NULL, ...)
}
\arguments{
\item{x}{\code{Satellite} or \code{Raster*} object.}

\item{mask}{\code{logical}. If \code{TRUE}, the cloudmask from the 
\code{Satellite} object (if available) will be considered in the regression 
model.}

\item{hillsh}{A \code{RasterLayer} created with \code{\link{hillShade}}.}

\item{cloudmask}{A \code{RasterLayer} in which clouds are masked with 
NA values, passed to \code{\link[raster]{mask}}.}

\item{...}{Additional arguments passed to \code{\link{writeRaster}}.}
}
\value{
If x is a Satellite object, a Satellite object with added, 
topographic corrected layers; if x is a \code{raster::Raster*} object, a 
\code{raster::Raster*} object with converted layer(s).
}
\description{
Correct for topographic effects.
}
\details{
The method of Civco (1989) is applied on atmospherically corrected bands 
(if not already available in the Satellite object, 
\code{\link{calcAtmosCorr}} is performed with its default settings.):
First, an analytical hillshade image is created based on a DEM and sun 
elevation and sun zenith information from the metadata. A regression between
the hillshade (independent variable) and each channel is then calculated 
with consideration of a cloudmask (if available).
The regression coefficents are used to calibrate the hillshade raster 
(for each channel individually). 
Finally, the calibrated hillshade image is subtracted from the corresponding
channel and the mean value of the channel is added.
}
\examples{
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
sat <- satellite(files)

## dem

files_dem <- list.files(path, pattern = "DEM", full.names = TRUE)
DEM <- raster(files_dem)

sat <- addSatDataLayer(sat, data = DEM, info = NULL, bcde = "DEM", in_bcde="DEM")

\dontrun{
sat <- calcTopoCorr(sat)
}
}
\references{
CIVCO, D.L. (1989): Topographic normalization of Landsat Thematic
Mapper digitalimagery. \emph{Photogrammetric Engineering & Remote Sensing}, 
55, 1303-1309.
}
