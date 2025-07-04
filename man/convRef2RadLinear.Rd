% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convRef2RadLinear.R
\name{convRef2RadLinear}
\alias{convRef2RadLinear}
\title{Convert reflectance to radiance using linear function coefficients}
\usage{
convRef2RadLinear(band, refm, refa, radm, rada, szen)
}
\arguments{
\item{band}{raster::RasterStack or raster::RasterLayer containing reflectance.}

\item{refm}{Multiplication coefficient for reflectance conversion.}

\item{refa}{Addtition coefficient for reflectance conversion.}

\item{radm}{Multiplication coefficient for radiance conversion.}

\item{rada}{Addition coefficient for radiance conversion.}

\item{szen}{Sun zenith angle.}
}
\value{
\code{raster::Raster*} object with converted values.
}
\description{
The function converts the reflectance (ref) back to radiance (rad) given that
linear conversion coefficients for both radiance and reflectance are 
available.
}
\details{
The conversion functions are taken from USGS' Landsat 8 Data Users
Handbook which is available online at 
\url{https://www.usgs.gov/landsat-missions/landsat-8-data-users-handbook}.
}
