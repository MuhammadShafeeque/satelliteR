% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alignGeometry.R
\name{alignGeometry}
\alias{alignGeometry}
\alias{alignGeometry,Satellite-method}
\alias{alignGeometry,RasterStack-method}
\alias{alignGeometry,RasterLayer-method}
\title{Align raster geometry between two data sets}
\usage{
\S4method{alignGeometry}{Satellite}(x, template, band_codes, type, method = c("bilinear", "ngb"))

\S4method{alignGeometry}{RasterStack}(x, template, method = c("bilinear", "ngb"))

\S4method{alignGeometry}{RasterLayer}(x, template, method = c("bilinear", "ngb"))
}
\arguments{
\item{x}{Satellite or Raster* object to be resampled.}

\item{template}{Raster* or spatial data set from which geometry can be 
extracted.}

\item{band_codes}{Band ID(s) to be resampled. If not supplied and type is 
not given, too, all bands will be considered for resampling.}

\item{type}{Type of bands (e.g. VIS, NIR) which should be considered. If not 
supplied, all types will be processed depending and bands to be processed can
be defined by band_codes.}

\item{method}{Method for resampling; "bilinear" for bilinear interpolation 
(default) or "ngb" for nearest neighbor interpolation. See e.g. 
\code{\link{resample}}, \code{\link{projectRaster}}.}
}
\value{
Satellite object with aligned geometries.

raster::RasterStack object with aligned layers

raster::RasterLayer object with aligned layer
}
\description{
Align raster data by bringing it in the same geometry and extent.
If the data set is not in the same projection as the template, the alignment
will be computed by reprojection. If the data has already the same
projection, the data set will be cropped and aggregated prior to resampling
in order to reduce computation time.
}
\examples{
path <- system.file("testdata/LC8", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
sat <- satellite(files)

alignGeometry(sat, template = getSatDataLayer(sat, "B008n"), 
               band_codes = "B001n")
}
