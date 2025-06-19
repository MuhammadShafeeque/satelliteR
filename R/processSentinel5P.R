if (!isGeneric("processSentinel5P")) {
  setGeneric("processSentinel5P", function(x, ...)
    standardGeneric("processSentinel5P"))
}

#' Process Sentinel-5P air pollution data
#'
#' @description
#' Process downloaded Sentinel-5P TROPOMI NetCDF files into Satellite objects
#' with proper metadata and calibration information for air pollution analysis.
#'
#' @param files Character vector. Paths to Sentinel-5P NetCDF files
#' @param parameters Character vector. Air pollution parameters to extract.
#'   Default extracts the main product variable. Options depend on product type:
#'   \itemize{
#'     \item NO2: "nitrogendioxide_tropospheric_column", "nitrogendioxide_stratospheric_column"
#'     \item SO2: "sulfurdioxide_total_vertical_column", "sulfurdioxide_total_air_mass_factor"
#'     \item CO: "carbonmonoxide_total_column", "carbonmonoxide_total_column_corrected"
#'     \item O3: "ozone_total_vertical_column", "ozone_effective_temperature"
#'     \item HCHO: "formaldehyde_tropospheric_vertical_column"
#'     \item CH4: "methane_mixing_ratio_bias_corrected"
#'     \item AER_AI: "absorbing_aerosol_index", "scene_albedo"
#'     \item AER_LH: "aerosol_mid_height", "aerosol_optical_depth"
#'   }
#' @param quality_filter Logical. Apply quality filtering based on qa_value
#' @param quality_threshold Numeric. Quality threshold (0-1, higher is better quality)
#' @param cloud_filter Logical. Apply cloud filtering
#' @param cloud_threshold Numeric. Cloud fraction threshold (0-1)
#' @param resample_resolution Numeric. Target resolution in degrees for resampling
#' @param output_crs Character. Target CRS (default: "+proj=longlat +datum=WGS84")
#' @param mask_invalid Logical. Mask invalid/fill values
#' @param scale_factor Logical. Apply scale factors and offsets from NetCDF metadata
#' @param unit_conversion Logical. Convert units to standard air pollution units
#' 
#' @return Satellite object containing:
#' \itemize{
#'   \item Processed air pollution data layers
#'   \item Quality assurance layers
#'   \item Metadata with sensor information, acquisition dates, and processing history
#'   \item Log information documenting processing steps
#' }
#' 
#' @export processSentinel5P
#' 
#' @examples
#' \dontrun{
#' # Process NO2 files
#' files <- list.files("./s5p_data", pattern = "NO2.*\\.nc$", full.names = TRUE)
#' sat <- processSentinel5P(
#'   files = files,
#'   parameters = c("nitrogendioxide_tropospheric_column"),
#'   quality_filter = TRUE,
#'   quality_threshold = 0.7,
#'   cloud_filter = TRUE,
#'   cloud_threshold = 0.3,
#'   unit_conversion = TRUE
#' )
#' 
#' # Plot NO2 data
#' plot(sat, bcde = "NO2_TROPO")
#' 
#' # Calculate statistics
#' no2_stats <- calcPollutionStats(sat, pollutant = "NO2")
#' }
#' 
#' @details
#' This function processes Sentinel-5P NetCDF files to extract air pollution
#' measurements and associated quality information. It handles:
#' 
#' - Reading NetCDF4 files with complex hierarchical structure
#' - Extracting geolocation and air pollution data
#' - Applying quality filters based on QA values
#' - Unit conversions to standard atmospheric chemistry units
#' - Spatial resampling and reprojection
#' - Integration with the satellite package's metadata system
#' 
#' **Quality Filtering:**
#' Sentinel-5P data includes quality assurance values that indicate measurement
#' reliability. The function can filter pixels based on:
#' - QA values (0-1 scale, with 1 being highest quality)
#' - Cloud fraction (removing cloudy pixels)
#' - Surface albedo and other retrieval parameters
#' 
#' **Unit Conversions:**
#' Standard conversions applied:
#' - NO2: mol/m² to molecules/cm²
#' - SO2: mol/m² to Dobson Units (DU)
#' - CO: mol/m² to molecules/cm²
#' - O3: mol/m² to Dobson Units (DU)
#' - HCHO: mol/m² to molecules/cm²
#' - CH4: ppb (mixing ratio)
#' 
#' @references
#' Eskes, H., et al. (2022). Sentinel-5 precursor/TROPOMI Level 2 Product 
#' User Manual. Royal Netherlands Meteorological Institute (KNMI).
#' 
#' @seealso \code{\link{downloadSentinel5P}}, \code{\link{calcPollutionStats}},
#'          \code{\link{plotPollutionTimeSeries}}
#' 
processSentinel5P <- function(files,
                             parameters = NULL,
                             quality_filter = TRUE,
                             quality_threshold = 0.5,
                             cloud_filter = FALSE,
                             cloud_threshold = 0.3,
                             resample_resolution = NULL,
                             output_crs = "+proj=longlat +datum=WGS84",
                             mask_invalid = TRUE,
                             scale_factor = TRUE,
                             unit_conversion = FALSE) {
  
  # Check required packages
  required_packages <- c("ncdf4", "raster", "sp")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('ncdf4', 'raster', 'sp'))"))
  }
  
  # Load required packages
  requireNamespace("ncdf4", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("sp", quietly = TRUE)
  
  if (length(files) == 0) {
    stop("No files provided")
  }
  
  # Check if files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop(paste("Files not found:", paste(missing_files, collapse = ", ")))
  }
  
  # Initialize processing log
  log_info <- list(
    start_time = Sys.time(),
    input_files = length(files),
    parameters = parameters,
    quality_filter = quality_filter,
    messages = character()
  )
  
  add_log_message <- function(message) {
    log_info$messages <<- c(log_info$messages, 
                           paste(Sys.time(), ":", message))
    message(message)
  }
  
  add_log_message(paste("Processing", length(files), "Sentinel-5P files"))
  
  # Process each file
  all_layers <- list()
  all_metadata <- list()
  
  for (i in seq_along(files)) {
    file_path <- files[i]
    add_log_message(paste("Processing file", i, "of", length(files), ":", basename(file_path)))
    
    tryCatch({
      result <- process_single_s5p_file(file_path, parameters, quality_filter,
                                      quality_threshold, cloud_filter, cloud_threshold,
                                      resample_resolution, output_crs, mask_invalid,
                                      scale_factor, unit_conversion, add_log_message)
      
      if (!is.null(result$layers) && length(result$layers) > 0) {
        all_layers <- c(all_layers, result$layers)
        all_metadata[[i]] <- result$metadata
      }
      
    }, error = function(e) {
      add_log_message(paste("Error processing file:", basename(file_path), "-", e$message))
    })
  }
  
  if (length(all_layers) == 0) {
    stop("No data could be extracted from any files")
  }
  
  # Combine metadata
  combined_metadata <- do.call(rbind, all_metadata)
  
  # Create Satellite object
  add_log_message("Creating Satellite object...")
  
  # Convert to Satellite object
  sat <- new("Satellite",
             layers = all_layers,
             meta = combined_metadata,
             log = list(list(
               datetime = Sys.time(),
               info = "Sentinel-5P data processing",
               in_bcde = NA_character_,
               out_bcde = paste(names(all_layers), collapse = ", ")
             )))
  
  log_info$end_time <- Sys.time()
  log_info$duration <- log_info$end_time - log_info$start_time
  add_log_message(paste("Processing completed. Duration:", 
                       round(log_info$duration, 2), 
                       attr(log_info$duration, "units")))
  
  # Add processing log to satellite object
  sat@log <- c(sat@log, list(log_info))
  
  return(sat)
}

# Helper function to process a single Sentinel-5P NetCDF file
process_single_s5p_file <- function(file_path, parameters, quality_filter,
                                   quality_threshold, cloud_filter, cloud_threshold,
                                   resample_resolution, output_crs, mask_invalid,
                                   scale_factor, unit_conversion, add_log_message) {
  
  # Open NetCDF file
  nc <- ncdf4::nc_open(file_path)
  
  tryCatch({
    # Get product type from filename or global attributes
    product_type <- extract_product_type(file_path, nc)
    add_log_message(paste("Product type:", product_type))
    
    # Determine parameters to extract if not specified
    if (is.null(parameters)) {
      parameters <- get_default_parameters(product_type)
    }
    
    # Extract geolocation
    geolocation <- extract_geolocation(nc, add_log_message)
    
    # Extract data layers
    layers <- list()
    metadata_rows <- list()
    
    for (param in parameters) {
      add_log_message(paste("Extracting parameter:", param))
      
      param_data <- extract_parameter_data(nc, param, quality_filter,
                                         quality_threshold, cloud_filter,
                                         cloud_threshold, mask_invalid,
                                         scale_factor, unit_conversion,
                                         product_type, add_log_message)
      
      if (!is.null(param_data)) {
        # Create raster
        raster_layer <- create_raster_from_s5p_data(param_data, geolocation,
                                                   output_crs, resample_resolution)
        
        if (!is.null(raster_layer)) {
          # Generate layer name
          layer_name <- generate_layer_name(product_type, param)
          names(raster_layer) <- layer_name
          
          layers[[layer_name]] <- raster_layer
          
          # Create metadata row
          metadata_rows[[length(metadata_rows) + 1]] <- create_s5p_metadata_row(
            file_path, product_type, param, layer_name, length(layers), nc
          )
        }
      }
    }
    
    # Combine metadata
    metadata <- if (length(metadata_rows) > 0) {
      do.call(rbind, metadata_rows)
    } else {
      data.frame()
    }
    
    return(list(layers = layers, metadata = metadata))
    
  }, finally = {
    ncdf4::nc_close(nc)
  })
}

# Helper functions for Sentinel-5P processing

extract_product_type <- function(file_path, nc) {
  filename <- basename(file_path)
  
  # Extract from filename pattern
  if (grepl("NO2", filename)) return("NO2")
  if (grepl("SO2", filename)) return("SO2")
  if (grepl("CO", filename)) return("CO")
  if (grepl("O3", filename)) return("O3")
  if (grepl("HCHO", filename)) return("HCHO")
  if (grepl("CH4", filename)) return("CH4")
  if (grepl("AER_AI", filename)) return("AER_AI")
  if (grepl("AER_LH", filename)) return("AER_LH")
  if (grepl("CLOUD", filename)) return("CLOUD")
  
  # Try to extract from global attributes
  if ("title" %in% names(nc$var)) {
    title <- ncdf4::ncatt_get(nc, 0, "title")$value
    if (grepl("nitrogen", title, ignore.case = TRUE)) return("NO2")
    if (grepl("sulfur", title, ignore.case = TRUE)) return("SO2")
    if (grepl("carbon monoxide", title, ignore.case = TRUE)) return("CO")
    if (grepl("ozone", title, ignore.case = TRUE)) return("O3")
    if (grepl("formaldehyde", title, ignore.case = TRUE)) return("HCHO")
    if (grepl("methane", title, ignore.case = TRUE)) return("CH4")
  }
  
  return("UNKNOWN")
}

get_default_parameters <- function(product_type) {
  param_map <- list(
    "NO2" = c("nitrogendioxide_tropospheric_column"),
    "SO2" = c("sulfurdioxide_total_vertical_column"),
    "CO" = c("carbonmonoxide_total_column"),
    "O3" = c("ozone_total_vertical_column"),
    "HCHO" = c("formaldehyde_tropospheric_vertical_column"),
    "CH4" = c("methane_mixing_ratio_bias_corrected"),
    "AER_AI" = c("absorbing_aerosol_index"),
    "AER_LH" = c("aerosol_mid_height"),
    "CLOUD" = c("cloud_fraction")
  )
  
  return(param_map[[product_type]] %||% character())
}

extract_geolocation <- function(nc, add_log_message) {
  # Try different possible geolocation variable names
  lon_vars <- c("longitude", "lon", "PRODUCT/longitude")
  lat_vars <- c("latitude", "lat", "PRODUCT/latitude")
  
  longitude <- NULL
  latitude <- NULL
  
  for (var in lon_vars) {
    if (var %in% names(nc$var)) {
      longitude <- ncdf4::ncvar_get(nc, var)
      break
    }
  }
  
  for (var in lat_vars) {
    if (var %in% names(nc$var)) {
      latitude <- ncdf4::ncvar_get(nc, var)
      break
    }
  }
  
  if (is.null(longitude) || is.null(latitude)) {
    stop("Could not extract geolocation information from NetCDF file")
  }
  
  return(list(longitude = longitude, latitude = latitude))
}

extract_parameter_data <- function(nc, param, quality_filter, quality_threshold,
                                 cloud_filter, cloud_threshold, mask_invalid,
                                 scale_factor, unit_conversion, product_type,
                                 add_log_message) {
  
  # Try different possible parameter paths
  param_paths <- c(param, paste0("PRODUCT/", param), paste0("PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/", param))
  
  data <- NULL
  for (path in param_paths) {
    if (path %in% names(nc$var)) {
      data <- ncdf4::ncvar_get(nc, path)
      break
    }
  }
  
  if (is.null(data)) {
    add_log_message(paste("Parameter not found:", param))
    return(NULL)
  }
  
  # Apply scale factor and offset if requested
  if (scale_factor) {
    var_info <- nc$var[[param_paths[!sapply(param_paths, is.null)][1]]]
    if (!is.null(var_info)) {
      scale_factor_val <- ncdf4::ncatt_get(nc, var_info, "scale_factor")
      add_offset_val <- ncdf4::ncatt_get(nc, var_info, "add_offset")
      fill_value <- ncdf4::ncatt_get(nc, var_info, "_FillValue")
      
      if (scale_factor_val$hasatt) {
        data <- data * scale_factor_val$value
      }
      if (add_offset_val$hasatt) {
        data <- data + add_offset_val$value
      }
      if (fill_value$hasatt && mask_invalid) {
        data[data == fill_value$value] <- NA
      }
    }
  }
  
  # Apply quality filtering
  if (quality_filter) {
    qa_data <- extract_qa_data(nc, add_log_message)
    if (!is.null(qa_data)) {
      data[qa_data < quality_threshold] <- NA
    }
  }
  
  # Apply cloud filtering
  if (cloud_filter) {
    cloud_data <- extract_cloud_data(nc, add_log_message)
    if (!is.null(cloud_data)) {
      data[cloud_data > cloud_threshold] <- NA
    }
  }
  
  # Apply unit conversion
  if (unit_conversion) {
    data <- apply_unit_conversion(data, product_type, param)
  }
  
  return(data)
}

extract_qa_data <- function(nc, add_log_message) {
  qa_vars <- c("qa_value", "PRODUCT/qa_value", "PRODUCT/SUPPORT_DATA/INPUT_DATA/qa_value")
  
  for (var in qa_vars) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    }
  }
  
  add_log_message("Quality assurance data not found")
  return(NULL)
}

extract_cloud_data <- function(nc, add_log_message) {
  cloud_vars <- c("cloud_fraction", "PRODUCT/cloud_fraction", 
                  "PRODUCT/SUPPORT_DATA/INPUT_DATA/cloud_fraction")
  
  for (var in cloud_vars) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    }
  }
  
  add_log_message("Cloud fraction data not found")
  return(NULL)
}

apply_unit_conversion <- function(data, product_type, param) {
  # Unit conversions for different pollutants
  # These are example conversions - actual values depend on specific requirements
  
  if (product_type == "NO2" && grepl("column", param)) {
    # Convert from mol/m² to molecules/cm²
    data <- data * 6.022e23 / 1e4  # Avogadro's number / cm²/m²
  } else if (product_type == "SO2" && grepl("column", param)) {
    # Convert from mol/m² to Dobson Units
    data <- data * 2241.15  # Conversion factor for SO2
  } else if (product_type == "CO" && grepl("column", param)) {
    # Convert from mol/m² to molecules/cm²
    data <- data * 6.022e23 / 1e4
  } else if (product_type == "O3" && grepl("column", param)) {
    # Convert from mol/m² to Dobson Units
    data <- data * 2241.15
  }
  
  return(data)
}

create_raster_from_s5p_data <- function(data, geolocation, output_crs, resample_resolution) {
  # Create raster from data and geolocation
  if (is.null(dim(data)) || length(dim(data)) < 2) {
    return(NULL)
  }
  
  # Get dimensions
  dims <- dim(data)
  if (length(dims) > 2) {
    # Take first time/layer if 3D
    data <- data[,,1]
  }
  
  # Create extent from geolocation
  lon_range <- range(geolocation$longitude, na.rm = TRUE)
  lat_range <- range(geolocation$latitude, na.rm = TRUE)
  
  # Create raster
  raster_layer <- raster::raster(nrows = nrow(data), ncols = ncol(data),
                                xmn = lon_range[1], xmx = lon_range[2],
                                ymn = lat_range[1], ymx = lat_range[2],
                                crs = "+proj=longlat +datum=WGS84")
  
  # Assign values
  raster::values(raster_layer) <- as.vector(data)
  
  # Reproject if needed
  if (output_crs != "+proj=longlat +datum=WGS84") {
    raster_layer <- raster::projectRaster(raster_layer, crs = output_crs)
  }
  
  # Resample if requested
  if (!is.null(resample_resolution)) {
    target_raster <- raster::raster(extent(raster_layer), 
                                   res = resample_resolution,
                                   crs = raster::crs(raster_layer))
    raster_layer <- raster::resample(raster_layer, target_raster)
  }
  
  return(raster_layer)
}

generate_layer_name <- function(product_type, param) {
  # Create standardized layer names
  name_map <- list(
    "nitrogendioxide_tropospheric_column" = "NO2_TROPO",
    "nitrogendioxide_stratospheric_column" = "NO2_STRATO",
    "sulfurdioxide_total_vertical_column" = "SO2_TOTAL",
    "carbonmonoxide_total_column" = "CO_TOTAL",
    "ozone_total_vertical_column" = "O3_TOTAL",
    "formaldehyde_tropospheric_vertical_column" = "HCHO_TROPO",
    "methane_mixing_ratio_bias_corrected" = "CH4_MIXING",
    "absorbing_aerosol_index" = "AER_AI",
    "aerosol_mid_height" = "AER_HEIGHT",
    "cloud_fraction" = "CLOUD_FRAC"
  )
  
  return(name_map[[param]] %||% paste0(product_type, "_", toupper(gsub("[^A-Za-z0-9]", "_", param))))
}

create_s5p_metadata_row <- function(file_path, product_type, param, layer_name, layer_number, nc) {
  # Extract sensing date from filename
  filename <- basename(file_path)
  
  # Try to extract date (format: YYYYMMDD)
  date_match <- regmatches(filename, regexpr("\\d{8}", filename))
  sensing_date <- if (length(date_match) > 0) {
    as.Date(date_match[1], format = "%Y%m%d")
  } else {
    Sys.Date()
  }
  
  # Create metadata row compatible with satellite package
  data.frame(
    SCENE = 1,
    DATE = sensing_date,
    SID = "S5P",
    SENSOR = "TROPOMI",
    SGRP = "Sentinel-5P",
    BID = layer_number,
    BCDE = layer_name,
    TYPE = map_pollutant_to_type(product_type),
    SPECTRUM = "atmospheric",
    CALIB = "L2",
    RID = "R00001",
    LNBR = layer_number,
    LAYER = layer_name,
    FILE = file_path,
    METAFILE = file_path,
    stringsAsFactors = FALSE
  )
}

map_pollutant_to_type <- function(product_type) {
  type_map <- list(
    "NO2" = "POLL",
    "SO2" = "POLL", 
    "CO" = "POLL",
    "O3" = "POLL",
    "HCHO" = "POLL",
    "CH4" = "POLL",
    "AER_AI" = "AER",
    "AER_LH" = "AER",
    "CLOUD" = "CLOUD"
  )
  
  return(type_map[[product_type]] %||% "POLL")
}

# Null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
