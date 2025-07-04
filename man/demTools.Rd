% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demTools.R
\name{demTools}
\alias{demTools}
\alias{demTools,Satellite-method}
\alias{demTools,RasterLayer-method}
\title{Compute terrain characteristics from digital elevation models}
\usage{
\S4method{demTools}{Satellite}(x, method = "hillShade", bcde = "DEM")

\S4method{demTools}{RasterLayer}(x, sunElev, sunAzim, method = "hillShade")
}
\arguments{
\item{x}{A DEM provided as an object of class Satellite or RasterLayer.}

\item{method}{Currently "slope", "aspect" and "hillshade" are implemented.}

\item{bcde}{The name of the DEM layer in the Satellite object.}

\item{sunElev}{If \code{method = "hillShade"}, the elevation angle of the 
sun in degrees. See parameter \code{angle} in \code{\link{hillShade}}.}

\item{sunAzim}{If \code{method = "hillShade"}, the sun azimuth angle in 
degree. See parameter \code{direction} in \code{\link{hillShade}}.}
}
\value{
If x is a Satellite object, a Satellite object with added layer containing calculated 
terrain information; if x is a \code{raster::RasterLayer} object, a 
\code{raster::RasterLayer} object with calculated terrain information.
}
\description{
Compute terrain characteristics from digital elevation models (DEM) using 
\code{raster::terrain} or \code{raster::hillShade}.
}
\examples{
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
sat <- satellite(files)

## dem
files_dem <- list.files(path, pattern = "DEM", full.names = TRUE)
DEM <- raster(files_dem)

sat <- addSatDataLayer(sat, data = DEM, info = NULL, bcde = "DEM", in_bcde="DEM")
sat <- demTools(sat)
}
\seealso{
\code{raster::terrain}, \code{raster::hillShade}.
}
