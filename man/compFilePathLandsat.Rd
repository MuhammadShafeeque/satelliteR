% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compFilePathLandsat.R
\name{compFilePathLandsat}
\alias{compFilePathLandsat}
\alias{sortFilesLandsat}
\title{Get filename, bands and metadata file for Landsat 7 and 8 standard 1B/T format}
\usage{
compFilePathLandsat(files)

sortFilesLandsat(files, id = FALSE)
}
\arguments{
\item{files}{Path and filename(s) of one or more Landsat band files or, 
alternatively, one or more Landsat metadata files.}

\item{id}{\code{logical}, defaults to \code{FALSE}. Determines whether to 
return sorted band files (ie default) or sorting order.}
}
\value{
\code{data.frame} containing filepaths, band numbers and metadata 
filepaths.

If \code{id = FALSE} (default), sorted band files as 
\code{character}, else the corresponding sorting order as \code{integer}.
}
\description{
The function compiles the sensor, band, filename and metadata filename information
for standard level 1B/T Landsat files.
}
\section{Functions}{
\itemize{
\item \code{sortFilesLandsat()}: Sort Landsat band files in ascending order.

}}
\examples{
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)

compFilePathLandsat(files)  

sortFilesLandsat(files)
sortFilesLandsat(files, id = TRUE) # indices

}
