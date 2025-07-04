#' Get or access internal LUT values used by various functions
#' 
#' @description
#' Get internal look-up table (LUT) values from sysdata.rda which have been 
#' compiled using data-raw/lut_data.R. Metadata is stored in \code{lut$meta}.
#' 
#' @param bcde Band code as returned e.g. from \code{\link{lutInfoBCDEFromBID}}.
#' @param bid Band id as returned e.g. from \code{\link{lutInfoBIDFromBCDE}}.
#' @param sid Sensor id as returned e.g. from \code{\link{lutInfoSensorFromSID}}.
#' @param file Filename of a remote sensing data file
#' @param files Filename (or filepath) of one or more remote sensing data 
#' filenames
#'
#' @return List containing several \code{data.frame} objects with LUT values.
#'
#' @export lutInfo
#' 
#' @details The functions above return the following information:
#' \itemize{
#'   \item \code{lutInfoBandsFromSID} returns the band info block.
#'   \item \code{lutInfoBCDEFromBID} returns the band code.
#'   \item \code{lutInfoBIDFromBCDE} returns the band ids.
#'   \item \code{lutInfoRSRromSID} returns the relative spectral response (rsr) 
#'         for the sensor.
#'   \item \code{lutInfoSensorFromSID} returns the sensor name.
#' }
#' 
#' The LUT contains the following band information taken, if not specified
#' otherwise, from the
#' \href{https://www.usgs.gov/faqs/what-are-band-designations-landsat-satellites?qt-news_science_products=0#qt-news_science_products}{USGS Landsat FAQ}:
#' \describe{
#'   \item{l4_band_wl}{Minimum/maximum wavelength for Landsat 4 bands.}
#'   \item{l5_band_wl}{Minimum/maximum wavelength for Landsat 5 bands.}
#'   \item{l7_band_wl}{Minimum/maximum wavelength for Landsat 7 bands.}
#' \item{l8_band_wl}{Minimum/maximum wavelength for Landsat 8 bands.}
#' \item{l7_rsr}{Landat 7 rsr (nm-1) taken from the
#' \href{https://landsat.usgs.gov/spectral-characteristics-viewer}{spectral viewer}
#' of the USGS Landsat FAQ.}
#' \item{l8_rsr}{Landat 8 rsr (nm-1) taken from the
#' \href{https://landsat.usgs.gov/spectral-characteristics-viewer}{spectral viewer}
#' of the USGS Landsat FAQ.}
#' \item{solar}{Solar irradiance (W m-2 nm-1) taken from the 
#' \href{http://rredc.nrel.gov/solar/spectra/am0/modtran.html}{National Renewable 
#' Energy Laboratory}.}
#' \item{l7_esun}{Tabulated ESun values from 
#' \href{https://www.usgs.gov/media/files/landsat-7-data-users-handbook}{tab 11.3 (Thuillier spectrum)}
#'  of the Landsat7 handbook.}
#' \item{l5_esun, l4_esun}{Tabulated ESun values from Chander G, Markham B 
#' (2003) Revised Landsat-5 TM radiometric calibration procedures and 
#' postcalibration dynamic ranges. IEEE Transaction on Geoscience and Remote 
#' Sensing 41/11, \doi{10.1109/LGRS.2007.898285}.}
#' }
#'  
#' @examples
#' ls_li <- lutInfo()
#' # str(ls_li)
#' 
lutInfo <- function(){
  return(lut)
}

# Return band information block ------------------------------------------------
#' @export lutInfoBandsFromSID
#'
#' @describeIn lutInfo
#' 
lutInfoBandsFromSID <- function(sid){
  return(
    lut[which(names(lut) == (lut$BANDS[which(names(lut$BANDS) == sid)]))][[1]])
}


# Return sensor ----------------------------------------------------------------
#' @export lutInfoSensorFromSID
#'
#' @describeIn lutInfo
#' 
lutInfoSensorFromSID <- function(sid){
  return(lut$SENSORS[which(names(lut$SENSORS) == sid)])
}


# Return band code -------------------------------------------------------------
#' @export lutInfoBCDEFromBID
#'
#' @describeIn lutInfo
#' 
lutInfoBCDEFromBID <- function(sid, bid){
  act_bands <- lutInfoBandsFromSID(sid)
  if(missing(bid)){
    return(act_bands$BCDE)
  } else {
    return(act_bands$BCDE[act_bands$BID == bid])
  }
}


# Return band ids --------------------------------------------------------------
#' @export lutInfoBIDFromBCDE
#'
#' @describeIn lutInfo
#' 
lutInfoBIDFromBCDE <- function(bcde, sid){
  act_bands <- lutInfoBandsFromSID(sid)
  return(act_bands$BID[act_bands$BCDE == bcde])
}


# Return rsr -------------------------------------------------------------------
#' @export lutInfoRSRromSID
#'
#' @describeIn lutInfo
#' 
lutInfoRSRromSID <- function(sid){
  return(lut[names(lut) == lut$RSR[which(names(lut$RSR) == sid)]][[1]])
}


# Return sensor id from filename --------------------------------------------------
#' @export lutInfoSIDfromFilename
#'
#' @describeIn lutInfo
#' 
lutInfoSIDfromFilename <- function(files){
  ## original sensor pattern
  test <- sapply(lut$SENSOR_ID_PATTERN$PATTERN, function(x) {
    grepl(x, basename(files))
  })
  
  ## alternative sensor pattern
  if (!any(test)) {
    test <- sapply(lut$SENSOR_ID_PATTERN$PATTERN2, function(x) {
      grepl(x, basename(files))
    })
  }
  
  if (inherits(test, "matrix")) {
    return(colnames(test)[test[1, ]])
  } else {
    return(names(test)[test])
  }
}


# Return group id from filename --------------------------------------------------
#' @export lutInfoSGRPfromFilename
#'
#' @describeIn lutInfo
#' 
lutInfoSGRPfromFilename <- function(file){
  sid <- lutInfoSIDfromFilename(file)
  if(length(sid) == 0){
    return(FALSE)
  } else {
    rid <- apply(lut$SENSOR_ID_PATTERN == sid, 1, FUN = any)
    return(lut$SENSOR_ID_PATTERN$SGRP[rid])  
  }
}
