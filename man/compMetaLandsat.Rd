% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compMetaLandsat.R
\name{compMetaLandsat}
\alias{compMetaLandsat}
\title{Get calibration information from Landsat 8 standard level 1B/T filename}
\usage{
compMetaLandsat(files)
}
\arguments{
\item{files}{Path and filename of the Landsat metadata file.}
}
\value{
\code{data.frame} containing the following information for each 
band/layer: 
\itemize{
  \item DATE date (e.g. 2013-07-07)
  \item SID sensor id (e.g. LC8)
  \item SENSOR sensor name (e.g. Landsat 8)
  \item SGRP sensor group (e.g. Landsat)
  \item BID band id (e.g. 7)
  \item BCDE band code (5 digit standard name, e.g B001n)
  \item SRES spatial resolution of the sensor band (e.g. 30 for 30 m x 30m)
  \item TYPE type of the sensor band regarding wavelength (e.g. VIS)
  \item SPECTRUM spectral range regarding radiation source (e.g. solar)
  \item CALIB type of applied calibration (e.g. SC for scaled counts)
  \item RID region id (e.g. R00001) for multi region Satellite objects
  \item RADA addtition coefficient for radiance conversion
  \item RADM multiplication coefficient for radiance conversion
  \item REFA addtition coefficient for reflectance conversion
  \item REFM multiplication coefficient for reflectance conversion
  \item BTK1 brightness temperature correction parameter
  \item BTK2 brightness temperature correction parameter
  \item SZEN sun zenith angle
  \item SAZM sun azimuth angle
  \item SELV sun elevation angle
  \item ESD earth-sun distance (AU)
  \item LMIN Minimum wavelength of the band (micrometer)
  \item LMAX Maximum wavelength of the band (micrometer)
  \item RADMIN Minimum radiance recorded by the band
  \item RADMAX Maximum radiance recorded by the band
  \item REFMIN Minimum reflectance recorded by the band
  \item REFMAX Maximum reflectance recorded by the band
  \item LNBR Layer number from 1 to n layers
  \item LAYER Layer name
  \item FILE Filepath of the data file
  \item METAFILE Filepath of the metadata file
}
}
\description{
The function scans a Lansat metadata file for various calibration 
and orbit coefficients as well as some sensor specific data.
}
\examples{
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
compMetaLandsat(files)

}
