% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPathRadDOS.R
\name{calcPathRadDOS}
\alias{calcPathRadDOS}
\alias{calcPathRadDOS,Satellite-method}
\alias{calcPathRadDOS,numeric-method}
\title{Compute path radiance based on the dark object method}
\usage{
\S4method{calcPathRadDOS}{Satellite}(x, model = c("DOS2", "DOS4"), esun_method = "RadRef")

\S4method{calcPathRadDOS}{numeric}(
  x,
  bnbr,
  band_wls,
  radm,
  rada,
  szen,
  esun,
  model = c("DOS2", "DOS4"),
  scat_coef = c(-4, -2, -1, -0.7, -0.5),
  dos_adjust = 0.01
)
}
\arguments{
\item{x}{A Satellite object or the value (scaled count) of a dark object in 
\code{bnbr} (e.g. minimum raw count of selected raster \code{bnbr}). If x is 
a Satellite object, the value is computed using \code{\link{calcDODN}}.}

\item{model}{Model to be used to correct for 1\% scattering (DOS2, DOS4; must 
be the same as used by \code{\link{calcAtmosCorr}}).}

\item{esun_method}{If x is a Satellite object, name of the method to be used 
to compute esun using one of \code{\link{calcTOAIrradRadRef}} ("RadRef"), 
\code{\link{calcTOAIrradTable}} ("Table") or \code{\link{calcTOAIrradModel}}
("Model")}

\item{bnbr}{Band number for which DNmin is valid.}

\item{band_wls}{Band wavelengths to be corrected; \code{data.frame} with min 
(max) in first (second) column, see details.}

\item{radm}{Multiplicative coefficient for radiance transformation (i.e. 
slope).}

\item{rada}{Additive coefficient for radiance transformation (i.e. offset).}

\item{szen}{Sun zenith angle.}

\item{esun}{Actual (i.e. non-normalized) TOA solar irradiance, e.g. returned 
by \code{\link{calcTOAIrradRadRef}}, \code{\link{calcTOAIrradTable}} or 
\code{\link{calcTOAIrradModel}}.}

\item{scat_coef}{Scattering coefficient; defaults to -4.0.}

\item{dos_adjust}{Assumed reflection for dark object adjustment; defaults to 0.01.}
}
\value{
Satellite object with path radiance for each band in the metadata
(W m-2 micrometer-1)

Vector object with path radiance values for each band 
(W m-2 micrometer-1)
}
\description{
Compute an estimated path radiance for all sensor bands, which can then be 
used to roughly correct the radiance values for atmospheric scattering. Path 
radiance estimation is based on a dark object method.
}
\details{
If x is a Satellite object, the minimum raw count value (x) is computed using
\code{\link{calcDODN}}. If the TOA solar irradiance is not part of the 
Satellite object's metadata, it is computed using 
\code{\link{calcTOAIrradRadRef}}, \code{\link{calcTOAIrradTable}} or 
\code{\link{calcTOAIrradModel}}.
 
The dark object subtraction approach is based on an approximation 
of the atmospheric path radiance (i.e. upwelling radiation which is 
scattered into the sensors field of view, aka haze) using the reflectance of a 
dark object (i.e. reflectance ~1\%). 

Chavez (1988) proposed a method which uses the dark object reflectance
in one band to predict the corresponding path radiances in all other 
\code{band_wls}. This is done using a relative radiance model which depends on 
the wavelength and overall atmospheric optical thickness (which is estimated 
based on the dark object's DN value). This has the advantage that the path 
radiance is actually correlated across different sensor \code{band_wls} and 
not computed individually for each band using independent dark objects. He 
proposed a relative radiance model which follows a wavelength dependent 
scattering that ranges from a power of -4 over -2, -1, -0.7 to -0.5 for very 
clear over clear, moderate, hazy to very hazy conditions. The relative 
factors are computed individually for each 1/1000 wavelength within each band 
range and subsequently averaged over the band as proposed by Goslee (2011).

The atmospheric transmittance towards the sensor (Tv) is approximated by 
1.0 (DOS2, Chavez 1996) or Rayleigh scattering (DOS4, Moran et al. 1992)

The atmospheric transmittance from the sun (Tz) is approximated by the 
cosine of the sun zenith angle (DOS2, Chavez 1996) or again using Rayleigh
scattering (DOS4, Moran et al. 1992).

The downwelling diffuse irradiance is approximated by 0.0 (DOS2, Chavez 1996)
or the hemispherical integral of the path radiance (DOS4, Moran et al. 1992).

Equations for the path radiance are taken from Song et al. (2001).

For some sensors, the band wavelengths are already included. See
\code{lutInfo()[grepl("_BANDS", names(lutInfo()$META))]} if your sensor is
included. To retrieve a sensor, use \code{lutInfo()$<Sensor ID>_BANDS}.
}
\examples{
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
sat <- satellite(files)
sat <- calcTOAIrradModel(sat)

bds <- "B002n"
val <- calcPathRadDOS(x = min(getValues(getSatDataLayer(sat, bds))),
                      bnbr = getSatLNBR(sat, bds),
                      band_wls = data.frame(LMIN = getSatLMIN(sat, getSatBCDESolar(sat)),
                                            LMAX = getSatLMAX(sat, getSatBCDESolar(sat))),
                      radm = getSatRADM(sat, getSatBCDESolar(sat)),
                      rada = getSatRADA(sat, getSatBCDESolar(sat)),
                      szen = getSatSZEN(sat, getSatBCDESolar(sat)),
                      esun = getSatESUN(sat, getSatBCDESolar(sat)),
                      model = "DOS2",
                      scat_coef = -4)
val
}
\references{
Chavez Jr PS (1988) An improved dark-object subtraction technique 
for atmospheric scattering correction of multispectral data. Remote Sensing 
of Environment 24/3, \doi{10.1016/0034-4257(88)90019-3}.
 
Chavez Jr PS (1996) Image-based atmospheric corrections revisited and
improved. Photogrammetric Engineering and Remote Sensing 62/9,
available online at 
\url{https://www.researchgate.net/publication/236769129_Image-Based_Atmospheric_Corrections_-_Revisited_and_Improved}.
 
Goslee SC (2011) Analyzing Remote Sensing Data in R: The landsat 
Package. Journal of Statistical Software, 43/4, 1-25, 
\doi{10.18637/jss.v043.i04}.

Moran MS, Jackson RD, Slater PN, Teillet PM (1992) Evlauation of simplified
procedures for rretrieval of land surface reflectane factors from satellite
sensor output.Remote Sensing of Environment 41/2-3, 169-184, 
\doi{10.1016/0034-4257(92)90076-V}.

Song C, Woodcock CE, Seto KC, Lenney MP, Macomber SA (2001) Classification 
and Change Detection Using Landsat TM Data: When and How to Correct 
Atmospheric Effects? Remote Sensing of Environment 75/2, 
\doi{10.1016/S0034-4257(00)00169-3}.

If you refer to Sawyer and Stephen 2014, please note that eq. 5 is wrong.
}
\seealso{
This function is used by \code{\link{calcAtmosCorr}} to 
compute the path radiance.
}
