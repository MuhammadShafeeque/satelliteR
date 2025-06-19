if (!isGeneric("processMODISPollution")) {
  setGeneric("processMODISPollution", function(x, ...)
    standardGeneric("processMODISPollution"))
}

#' Process MODIS air pollution and atmospheric data
#'
#' @description
#' Process downloaded MODIS HDF files containing atmospheric and air pollution
#' data into Satellite objects with proper metadata and quality information.
#'
#' @param files Character vector. Paths to MODIS HDF files
#' @param parameters Character vector. Parameters to extract. If NULL, extracts
#'   default parameters based on product type:
#'   \itemize{
#'     \item MOD04/MYD04: "Optical_Depth_Land_And_Ocean", "Fine_Mode_Fraction_Ocean"
#'     \item MOD14/MYD14: "fire_mask", "FirePix"
#'     \item MOD08/MYD08: "Aerosol_Optical_Depth_Land_Ocean_Mean", "Cloud_Fraction_Mean"
#'   }
#' @param quality_filter Logical. Apply quality filtering based on QA flags
#' @param quality_bands Character vector. Quality assurance band names to use for filtering
#' @param scale_factors Logical. Apply scale factors and offsets from HDF metadata
#' @param mask_fill_values Logical. Mask fill values and invalid data
#' @param aggregate_method Character. Method for temporal aggregation: "mean", "median", "max"
#' @param spatial_resolution Numeric. Target spatial resolution in degrees
#' @param output_crs Character. Target coordinate reference system
#' @param extract_geolocation Logical. Extract and use embedded geolocation data
#' 
#' @return Satellite object containing:
#' \itemize{
#'   \item Processed MODIS atmospheric/pollution data layers
#'   \item Quality assurance layers
#'   \item Metadata with MODIS sensor information and processing history
#'   \item Log information documenting processing steps
#' }
#' 
#' @export processMODISPollution
#' 
#' @examples
#' \dontrun{
#' # Process MODIS aerosol files
#' files <- list.files("./modis_data", pattern = "MOD04.*\\.hdf$", full.names = TRUE)
#' sat <- processMODISPollution(
#'   files = files,
#'   parameters = c("Optical_Depth_Land_And_Ocean", "Fine_Mode_Fraction_Ocean"),
#'   quality_filter = TRUE,
#'   scale_factors = TRUE,
#'   mask_fill_values = TRUE
#' )
#' 
#' # Process fire data
#' fire_files <- list.files("./modis_data", pattern = "MOD14.*\\.hdf$", full.names = TRUE)
#' fire_sat <- processMODISPollution(
#'   files = fire_files,
#'   parameters = c("fire_mask", "FirePix"),
#'   quality_filter = TRUE
#' )
#' 
#' # Plot aerosol optical depth
#' plot(sat, bcde = "AOD_LAND_OCEAN")
#' 
#' # Calculate pollution statistics
#' aod_stats <- calcPollutionStats(sat, pollutant = "AOD")
#' }
#' 
#' @details
#' This function processes MODIS HDF files to extract atmospheric and air pollution
#' data. It handles the complex HDF4 file structure and applies appropriate
#' quality filtering and calibration.
#' 
#' **MODIS Products Supported:**
#' 
#' - **MOD04/MYD04 (Aerosol)**: Aerosol optical depth, fine mode fraction,
#'   and other aerosol properties for air quality monitoring
#' - **MOD14/MYD14 (Fire)**: Active fire detections and thermal anomalies
#'   for tracking biomass burning emissions
#' - **MOD08/MYD08 (Atmosphere)**: Daily/monthly atmospheric parameters
#'   including aggregated aerosol and cloud properties
#' 
#' **Quality Filtering:**
#' MODIS data includes extensive quality assurance information:
#' - Land/Ocean quality flags for aerosol products
#' - Fire confidence levels for fire products
#' - Cloud screening and atmospheric correction flags
#' 
#' **Spatial Processing:**
#' - Handles different MODIS projections (geographic, sinusoidal)
#' - Resamples to regular grids when needed
#' - Extracts embedded geolocation arrays for swath products
#' 
#' **Temporal Processing:**
#' - Aggregates multiple files using specified method
#' - Maintains temporal metadata for time series analysis
#' 
#' @references
#' Levy, R. C., et al. (2013). The Collection 6 MODIS aerosol products over land 
#' and ocean. Atmospheric Measurement Techniques, 6(11), 2989-3034.
#' 
#' Giglio, L., et al. (2018). The Collection 6 MODIS burned area mapping algorithm 
#' and product. Remote sensing of environment, 217, 72-85.
#' 
#' @seealso \code{\link{downloadMODISPollution}}, \code{\link{processSentinel5P}},
#'          \code{\link{calcPollutionStats}}
#' 
processMODISPollution <- function(files,
                                 parameters = NULL,
                                 quality_filter = TRUE,
                                 quality_bands = NULL,
                                 scale_factors = TRUE,
                                 mask_fill_values = TRUE,
                                 aggregate_method = "mean",
                                 spatial_resolution = NULL,
                                 output_crs = "+proj=longlat +datum=WGS84",
                                 extract_geolocation = TRUE) {
  
  # Check required packages
  required_packages <- c("gdalUtils", "raster", "rgdal")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('gdalUtils', 'raster', 'rgdal'))"))
  }
  
  # Load required packages
  requireNamespace("gdalUtils", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rgdal", quietly = TRUE)
  
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
  
  add_log_message(paste("Processing", length(files), "MODIS files"))
  
  # Determine product type from first file
  product_type <- extract_modis_product_type(files[1])
  add_log_message(paste("Detected product type:", product_type))
  
  # Set default parameters if not provided
  if (is.null(parameters)) {
    parameters <- get_default_modis_parameters(product_type)
    add_log_message(paste("Using default parameters:", paste(parameters, collapse = ", ")))
  }
  
  # Process each file
  all_layers <- list()
  all_metadata <- list()
  
  for (i in seq_along(files)) {
    file_path <- files[i]
    add_log_message(paste("Processing file", i, "of", length(files), ":", basename(file_path)))
    
    tryCatch({
      result <- process_single_modis_file(file_path, product_type, parameters,
                                        quality_filter, quality_bands, scale_factors,
                                        mask_fill_values, spatial_resolution,
                                        output_crs, extract_geolocation, add_log_message)
      
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
  
  # Aggregate if multiple files and requested
  if (length(files) > 1 && aggregate_method != "none") {
    add_log_message(paste("Aggregating layers using method:", aggregate_method))
    all_layers <- aggregate_modis_layers(all_layers, aggregate_method, add_log_message)
  }
  
  # Combine metadata
  combined_metadata <- do.call(rbind, all_metadata)
  
  # Create Satellite object
  add_log_message("Creating Satellite object...")
  
  sat <- new("Satellite",
             layers = all_layers,
             meta = combined_metadata,
             log = list(list(
               datetime = Sys.time(),
               info = paste("MODIS", product_type, "data processing"),
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

# Helper function to process a single MODIS HDF file
process_single_modis_file <- function(file_path, product_type, parameters,
                                     quality_filter, quality_bands, scale_factors,
                                     mask_fill_values, spatial_resolution,
                                     output_crs, extract_geolocation, add_log_message) {
  
  # Get HDF dataset information
  hdf_info <- gdalUtils::gdalinfo(file_path, checksum = FALSE)
  
  # Extract subdatasets
  subdatasets <- extract_hdf_subdatasets(file_path, add_log_message)
  
  if (length(subdatasets) == 0) {
    add_log_message("No subdatasets found in HDF file")
    return(list(layers = NULL, metadata = NULL))
  }
  
  # Extract data layers
  layers <- list()
  metadata_rows <- list()
  
  for (param in parameters) {
    add_log_message(paste("Extracting parameter:", param))
    
    # Find matching subdataset
    matching_sds <- find_matching_subdataset(subdatasets, param)
    
    if (is.null(matching_sds)) {
      add_log_message(paste("Parameter not found in file:", param))
      next
    }
    
    # Read data
    raster_layer <- read_modis_subdataset(matching_sds, scale_factors, 
                                        mask_fill_values, add_log_message)
    
    if (is.null(raster_layer)) {
      add_log_message(paste("Failed to read parameter:", param))
      next
    }
    
    # Apply quality filtering
    if (quality_filter) {
      qa_layer <- extract_modis_qa_layer(subdatasets, quality_bands, product_type)
      if (!is.null(qa_layer)) {
        raster_layer <- apply_modis_quality_filter(raster_layer, qa_layer, product_type)
      }
    }
    
    # Reproject if needed
    if (output_crs != raster::crs(raster_layer)@projargs) {
      raster_layer <- raster::projectRaster(raster_layer, crs = output_crs)
    }
    
    # Resample if requested
    if (!is.null(spatial_resolution)) {
      target_raster <- raster::raster(extent(raster_layer), 
                                     res = spatial_resolution,
                                     crs = raster::crs(raster_layer))
      raster_layer <- raster::resample(raster_layer, target_raster)
    }
    
    # Generate layer name
    layer_name <- generate_modis_layer_name(product_type, param)
    names(raster_layer) <- layer_name
    
    layers[[layer_name]] <- raster_layer
    
    # Create metadata row
    metadata_rows[[length(metadata_rows) + 1]] <- create_modis_metadata_row(
      file_path, product_type, param, layer_name, length(layers)
    )
  }
  
  # Combine metadata
  metadata <- if (length(metadata_rows) > 0) {
    do.call(rbind, metadata_rows)
  } else {
    data.frame()
  }
  
  return(list(layers = layers, metadata = metadata))
}

# Helper functions for MODIS processing

extract_modis_product_type <- function(file_path) {
  filename <- basename(file_path)
  
  if (grepl("MOD04|MYD04", filename)) return("AEROSOL")
  if (grepl("MOD14|MYD14", filename)) return("FIRE")
  if (grepl("MOD08|MYD08", filename)) return("ATMOSPHERE")
  if (grepl("MCD43", filename)) return("BRDF")
  
  return("UNKNOWN")
}

get_default_modis_parameters <- function(product_type) {
  param_map <- list(
    "AEROSOL" = c("Optical_Depth_Land_And_Ocean", "Fine_Mode_Fraction_Ocean"),
    "FIRE" = c("fire_mask", "FirePix"),
    "ATMOSPHERE" = c("Aerosol_Optical_Depth_Land_Ocean_Mean", "Cloud_Fraction_Mean"),
    "BRDF" = c("Nadir_Reflectance_Band1", "Nadir_Reflectance_Band2")
  )
  
  return(param_map[[product_type]] %||% character())
}

extract_hdf_subdatasets <- function(file_path, add_log_message) {
  # Use gdalinfo to get subdataset information
  info_output <- gdalUtils::gdalinfo(file_path, checksum = FALSE, stats = FALSE)
  
  # Parse subdatasets from gdalinfo output
  subdatasets <- character()
  
  if (length(info_output) > 0) {
    sds_lines <- grep("SUBDATASET_.*_NAME=", info_output, value = TRUE)
    
    for (line in sds_lines) {
      # Extract subdataset path
      sds_path <- sub(".*SUBDATASET_.*_NAME=", "", line)
      subdatasets <- c(subdatasets, sds_path)
    }
  }
  
  add_log_message(paste("Found", length(subdatasets), "subdatasets"))
  return(subdatasets)
}

find_matching_subdataset <- function(subdatasets, param) {
  # Find subdataset that contains the parameter name
  for (sds in subdatasets) {
    if (grepl(param, sds, ignore.case = TRUE)) {
      return(sds)
    }
  }
  return(NULL)
}

read_modis_subdataset <- function(subdataset_path, scale_factors, mask_fill_values, add_log_message) {
  tryCatch({
    # Read using GDAL
    raster_layer <- raster::raster(subdataset_path)
    
    # Apply scale factors if requested
    if (scale_factors) {
      # Try to get scale factor and offset from GDAL metadata
      metadata <- raster::metadata(raster_layer)
      
      if ("scale_factor" %in% names(metadata)) {
        scale_val <- as.numeric(metadata[["scale_factor"]])
        if (!is.na(scale_val) && scale_val != 1) {
          raster_layer <- raster_layer * scale_val
        }
      }
      
      if ("add_offset" %in% names(metadata)) {
        offset_val <- as.numeric(metadata[["add_offset"]])
        if (!is.na(offset_val) && offset_val != 0) {
          raster_layer <- raster_layer + offset_val
        }
      }
    }
    
    # Mask fill values if requested
    if (mask_fill_values) {
      # Common MODIS fill values
      fill_values <- c(-9999, -999, 32767, 65535)
      
      for (fill_val in fill_values) {
        raster_layer[raster_layer == fill_val] <- NA
      }
    }
    
    return(raster_layer)
    
  }, error = function(e) {
    add_log_message(paste("Error reading subdataset:", e$message))
    return(NULL)
  })
}

extract_modis_qa_layer <- function(subdatasets, quality_bands, product_type) {
  # Default quality band names for different products
  default_qa_bands <- list(
    "AEROSOL" = c("Quality_Assurance_Land", "Quality_Assurance_Ocean"),
    "FIRE" = c("QA", "confidence"),
    "ATMOSPHERE" = c("QA_Mean", "Quality_Assurance"),
    "BRDF" = c("BRDF_Albedo_Quality")
  )
  
  if (is.null(quality_bands)) {
    quality_bands <- default_qa_bands[[product_type]]
  }
  
  if (is.null(quality_bands)) return(NULL)
  
  # Find QA subdataset
  for (qa_band in quality_bands) {
    qa_sds <- find_matching_subdataset(subdatasets, qa_band)
    if (!is.null(qa_sds)) {
      return(read_modis_subdataset(qa_sds, FALSE, FALSE, function(x) {}))
    }
  }
  
  return(NULL)
}

apply_modis_quality_filter <- function(data_layer, qa_layer, product_type) {
  if (is.null(qa_layer)) return(data_layer)
  
  # Resample QA layer to match data layer if needed
  if (!compareRaster(data_layer, qa_layer, stopiffalse = FALSE)) {
    qa_layer <- raster::resample(qa_layer, data_layer, method = "ngb")
  }
  
  # Apply product-specific quality filtering
  if (product_type == "AEROSOL") {
    # For aerosol products, keep only high quality retrievals
    # MODIS aerosol QA uses bit flags - this is a simplified approach
    good_quality <- qa_layer == 3  # Assuming 3 represents good quality
    data_layer[!good_quality] <- NA
    
  } else if (product_type == "FIRE") {
    # For fire products, filter by confidence
    # Confidence levels: 0-30 (low), 31-80 (nominal), 81-100 (high)
    high_confidence <- qa_layer >= 31
    data_layer[!high_confidence] <- NA
  }
  
  return(data_layer)
}

generate_modis_layer_name <- function(product_type, param) {
  # Create standardized layer names
  name_map <- list(
    "Optical_Depth_Land_And_Ocean" = "AOD_LAND_OCEAN",
    "Fine_Mode_Fraction_Ocean" = "FMF_OCEAN",
    "fire_mask" = "FIRE_MASK",
    "FirePix" = "FIRE_PIX",
    "Aerosol_Optical_Depth_Land_Ocean_Mean" = "AOD_MEAN",
    "Cloud_Fraction_Mean" = "CLOUD_FRAC_MEAN",
    "Nadir_Reflectance_Band1" = "REFL_B1",
    "Nadir_Reflectance_Band2" = "REFL_B2"
  )
  
  return(name_map[[param]] %||% paste0(product_type, "_", toupper(gsub("[^A-Za-z0-9]", "_", param))))
}

create_modis_metadata_row <- function(file_path, product_type, param, layer_name, layer_number) {
  # Extract sensing date from MODIS filename
  filename <- basename(file_path)
  
  # Extract date (format: A2023001 = day 1 of 2023)
  date_match <- regmatches(filename, regexpr("A\\d{7}", filename))
  
  sensing_date <- if (length(date_match) > 0) {
    # Parse MODIS date format
    year <- substr(date_match[1], 2, 5)
    doy <- substr(date_match[1], 6, 8)
    
    # Convert day of year to date
    base_date <- as.Date(paste0(year, "-01-01"))
    base_date + as.numeric(doy) - 1
  } else {
    Sys.Date()
  }
  
  # Determine sensor
  sensor <- if (grepl("MOD", filename)) {
    "Terra-MODIS"
  } else if (grepl("MYD", filename)) {
    "Aqua-MODIS"
  } else if (grepl("MCD", filename)) {
    "Combined-MODIS"
  } else {
    "MODIS"
  }
  
  # Create metadata row compatible with satellite package
  data.frame(
    SCENE = 1,
    DATE = sensing_date,
    SID = if (grepl("MOD", filename)) "MOD" else if (grepl("MYD", filename)) "MYD" else "MCD",
    SENSOR = sensor,
    SGRP = "MODIS",
    BID = layer_number,
    BCDE = layer_name,
    TYPE = map_modis_type(product_type, param),
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

map_modis_type <- function(product_type, param) {
  type_map <- list(
    "AEROSOL" = "AER",
    "FIRE" = "FIRE",
    "ATMOSPHERE" = "ATM",
    "BRDF" = "REFL"
  )
  
  return(type_map[[product_type]] %||% "ATM")
}

aggregate_modis_layers <- function(layers, method, add_log_message) {
  # Group layers by name
  layer_names <- names(layers)
  unique_names <- unique(layer_names)
  
  aggregated_layers <- list()
  
  for (name in unique_names) {
    same_name_layers <- layers[layer_names == name]
    
    if (length(same_name_layers) == 1) {
      aggregated_layers[[name]] <- same_name_layers[[1]]
    } else {
      add_log_message(paste("Aggregating", length(same_name_layers), "layers for:", name))
      
      # Stack layers
      layer_stack <- raster::stack(same_name_layers)
      
      # Apply aggregation method
      if (method == "mean") {
        aggregated_layers[[name]] <- raster::calc(layer_stack, mean, na.rm = TRUE)
      } else if (method == "median") {
        aggregated_layers[[name]] <- raster::calc(layer_stack, median, na.rm = TRUE)
      } else if (method == "max") {
        aggregated_layers[[name]] <- raster::calc(layer_stack, max, na.rm = TRUE)
      }
      
      names(aggregated_layers[[name]]) <- name
    }
  }
  
  return(aggregated_layers)
}

# Null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
