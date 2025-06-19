if (!isGeneric("downloadMODISPollution")) {
  setGeneric("downloadMODISPollution", function(x, ...)
    standardGeneric("downloadMODISPollution"))
}

#' Download MODIS air pollution and atmospheric data
#'
#' @description
#' Download MODIS atmospheric data including aerosol optical depth, fire data,
#' and other air quality related products from NASA's data sources.
#'
#' @param product Character. MODIS product to download. Options include:
#' \itemize{
#'   \item "MOD04_L2" - Aerosol 5-Min L2 Swath 10km (Terra)
#'   \item "MYD04_L2" - Aerosol 5-Min L2 Swath 10km (Aqua)
#'   \item "MOD04_3K" - Aerosol 5-Min L2 Swath 3km (Terra)
#'   \item "MYD04_3K" - Aerosol 5-Min L2 Swath 3km (Aqua)
#'   \item "MOD14A1" - Thermal Anomalies/Fire Daily L3 Global 1km (Terra)
#'   \item "MYD14A1" - Thermal Anomalies/Fire Daily L3 Global 1km (Aqua)
#'   \item "MOD08_D3" - Atmosphere Daily L3 Global 1deg (Terra)
#'   \item "MYD08_D3" - Atmosphere Daily L3 Global 1deg (Aqua)
#'   \item "MOD08_M3" - Atmosphere Monthly L3 Global 1deg (Terra)
#'   \item "MYD08_M3" - Atmosphere Monthly L3 Global 1deg (Aqua)
#'   \item "MCD43A4" - BRDF/Albedo Nadir Daily L3 Global 500m
#' }
#' @param start_date Character. Start date in format "YYYY-MM-DD"
#' @param end_date Character. End date in format "YYYY-MM-DD"
#' @param bbox Numeric vector. Bounding box as c(xmin, ymin, xmax, ymax) in WGS84
#' @param output_dir Character. Directory to save downloaded files
#' @param source Character. Data source: "earthdata", "modis_tools", or "auto"
#' @param username Character. NASA Earthdata username
#' @param password Character. NASA Earthdata password
#' @param max_files Integer. Maximum number of files to download
#' @param quality_day Logical. For fire products, download only day overpasses
#' @param collection Character. MODIS collection version (e.g., "6.1", "6")
#' 
#' @return List containing:
#' \itemize{
#'   \item downloaded_files: Character vector of downloaded file paths
#'   \item metadata: Data frame with product metadata
#'   \item log: Processing log information
#' }
#' 
#' @export downloadMODISPollution
#' 
#' @examples
#' \dontrun{
#' # Download MODIS aerosol data
#' result <- downloadMODISPollution(
#'   product = "MOD04_L2",
#'   start_date = "2023-06-01",
#'   end_date = "2023-06-07",
#'   bbox = c(-125, 30, -110, 45), # California
#'   output_dir = "./modis_data",
#'   source = "earthdata",
#'   username = "your_username",
#'   password = "your_password",
#'   max_files = 20
#' )
#' 
#' # Download fire data
#' fire_result <- downloadMODISPollution(
#'   product = "MOD14A1",
#'   start_date = "2023-08-01",
#'   end_date = "2023-08-31",
#'   bbox = c(-124, 32, -114, 42), # California fire season
#'   quality_day = TRUE
#' )
#' 
#' # Process downloaded files
#' sat <- processMODISPollution(result$downloaded_files)
#' }
#' 
#' @details
#' This function provides access to MODIS atmospheric and air pollution data
#' through multiple sources:
#' 
#' 1. **NASA Earthdata**: Direct access to NASA's official data distribution
#' 2. **MODIS Tools**: Using the MODISTools R package for simplified access
#' 3. **Auto**: Automatically selects the best available source
#' 
#' **MODIS Products for Air Quality:**
#' 
#' - **Aerosol Products (MOD/MYD04)**: Aerosol optical depth, fine mode fraction,
#'   and other aerosol properties important for air quality assessment
#' - **Fire Products (MOD/MYD14)**: Active fire detections and thermal anomalies
#'   useful for tracking biomass burning emissions
#' - **Atmospheric Products (MOD/MYD08)**: Daily and monthly atmospheric parameters
#'   including aerosol optical depth, cloud properties, and water vapor
#' 
#' **Authentication:**
#' For NASA Earthdata access, you need to register at:
#' https://urs.earthdata.nasa.gov/users/new
#' 
#' **Data Availability:**
#' - Terra MODIS: 2000-present
#' - Aqua MODIS: 2002-present
#' - Spatial resolution: 1km to 1 degree depending on product
#' - Temporal resolution: Daily to monthly depending on product
#' 
#' @references
#' Kaufman, Y. J., et al. (1997). Operational remote sensing of tropospheric 
#' aerosol over land from EOS moderate resolution imaging spectroradiometer. 
#' Journal of Geophysical Research, 102(D14), 17051-17067.
#' 
#' Justice, C. O., et al. (2002). The MODIS fire products. Remote sensing of 
#' environment, 83(1-2), 244-262.
#' 
#' @seealso \code{\link{processMODISPollution}}, \code{\link{downloadSentinel5P}}
#' 
downloadMODISPollution <- function(product = "MOD04_L2",
                                  start_date,
                                  end_date,
                                  bbox,
                                  output_dir = "./modis_data",
                                  source = "auto",
                                  username = NULL,
                                  password = NULL,
                                  max_files = 100,
                                  quality_day = FALSE,
                                  collection = "6.1") {
  
  # Validate inputs
  if (missing(start_date) || missing(end_date)) {
    stop("start_date and end_date are required")
  }
  
  if (missing(bbox) || length(bbox) != 4) {
    stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
  }
  
  # Validate product type
  valid_products <- c("MOD04_L2", "MYD04_L2", "MOD04_3K", "MYD04_3K",
                      "MOD14A1", "MYD14A1", "MOD08_D3", "MYD08_D3",
                      "MOD08_M3", "MYD08_M3", "MCD43A4")
  
  if (!product %in% valid_products) {
    stop(paste("Invalid product. Must be one of:", paste(valid_products, collapse = ", ")))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize log
  log_info <- list(
    start_time = Sys.time(),
    product = product,
    start_date = start_date,
    end_date = end_date,
    bbox = bbox,
    source = source,
    collection = collection,
    messages = character()
  )
  
  # Add message to log
  add_log_message <- function(message) {
    log_info$messages <<- c(log_info$messages, 
                           paste(Sys.time(), ":", message))
    message(message)
  }
  
  add_log_message(paste("Starting MODIS download for product:", product))
  
  # Determine source
  if (source == "auto") {
    # Check for available sources and select the best one
    if (!is.null(username) && !is.null(password)) {
      source <- "earthdata"
      add_log_message("Auto-selected NASA Earthdata (credentials provided)")
    } else if (requireNamespace("MODISTools", quietly = TRUE)) {
      source <- "modis_tools"
      add_log_message("Auto-selected MODISTools (package available)")
    } else {
      stop("No viable data source available. Please provide NASA Earthdata credentials or install MODISTools package.")
    }
  }
  
  # Download based on source
  downloaded_files <- character()
  metadata <- data.frame()
  
  if (source == "earthdata") {
    result <- download_from_earthdata(product, start_date, end_date, bbox,
                                    output_dir, username, password, max_files,
                                    quality_day, collection, add_log_message)
    downloaded_files <- result$files
    metadata <- result$metadata
    
  } else if (source == "modis_tools") {
    result <- download_with_modis_tools(product, start_date, end_date, bbox,
                                      output_dir, max_files, quality_day,
                                      collection, add_log_message)
    downloaded_files <- result$files
    metadata <- result$metadata
    
  } else {
    stop(paste("Unsupported source:", source))
  }
  
  log_info$end_time <- Sys.time()
  log_info$duration <- log_info$end_time - log_info$start_time
  add_log_message(paste("Download completed. Duration:", 
                       round(log_info$duration, 2), 
                       attr(log_info$duration, "units")))
  
  return(list(
    downloaded_files = downloaded_files,
    metadata = metadata,
    log = log_info
  ))
}

# Helper function for NASA Earthdata downloads
download_from_earthdata <- function(product, start_date, end_date, bbox,
                                   output_dir, username, password, max_files,
                                   quality_day, collection, add_log_message) {
  
  add_log_message("Connecting to NASA Earthdata...")
  
  if (is.null(username) || is.null(password)) {
    stop("Username and password required for NASA Earthdata access")
  }
  
  # Check if required packages are available
  required_packages <- c("httr", "xml2", "jsonlite")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('httr', 'xml2', 'jsonlite'))"))
  }
  
  # Load required packages
  requireNamespace("httr", quietly = TRUE)
  requireNamespace("xml2", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  
  # NASA Earthdata search endpoints
  cmr_base_url <- "https://cmr.earthdata.nasa.gov/search"
  
  # Format dates for CMR
  start_date_formatted <- paste0(start_date, "T00:00:00Z")
  end_date_formatted <- paste0(end_date, "T23:59:59Z")
  
  # Create bounding box string
  bbox_string <- paste(bbox, collapse = ",")
  
  # Map product to short name and platform
  product_info <- get_modis_product_info(product)
  
  # Search for granules
  search_params <- list(
    short_name = product_info$short_name,
    version = collection,
    temporal = paste0(start_date_formatted, ",", end_date_formatted),
    bounding_box = bbox_string,
    page_size = min(max_files, 2000),
    page_num = 1
  )
  
  if (!is.null(product_info$platform)) {
    search_params$platform <- product_info$platform
  }
  
  add_log_message("Searching for MODIS granules...")
  
  # Perform search
  search_url <- paste0(cmr_base_url, "/granules.json")
  response <- httr::GET(search_url, query = search_params)
  
  if (httr::status_code(response) != 200) {
    stop(paste("Search failed with status code:", httr::status_code(response)))
  }
  
  # Parse response
  search_results <- httr::content(response, "text", encoding = "UTF-8")
  results_json <- jsonlite::fromJSON(search_results)
  
  if (length(results_json$feed$entry) == 0) {
    add_log_message("No granules found matching criteria")
    return(list(files = character(), metadata = data.frame()))
  }
  
  granules <- results_json$feed$entry
  add_log_message(paste("Found", nrow(granules), "granules"))
  
  # Filter for day passes if requested
  if (quality_day && grepl("14", product)) {
    day_granules <- granules[grepl("Day", granules$title, ignore.case = TRUE), ]
    if (nrow(day_granules) > 0) {
      granules <- day_granules
      add_log_message(paste("Filtered to", nrow(granules), "day-pass granules"))
    }
  }
  
  # Limit number of granules
  if (nrow(granules) > max_files) {
    granules <- granules[1:max_files, ]
    add_log_message(paste("Limited to", max_files, "granules"))
  }
  
  downloaded_files <- character()
  metadata_list <- list()
  
  # Download each granule
  for (i in seq_len(nrow(granules))) {
    granule <- granules[i, ]
    granule_id <- granule$id
    granule_title <- granule$title
    
    add_log_message(paste("Downloading granule", i, "of", nrow(granules), ":", granule_title))
    
    # Get download URL
    if ("links" %in% names(granule) && length(granule$links) > 0) {
      download_links <- granule$links
      
      # Find HDF download link
      hdf_link <- NULL
      if (is.data.frame(download_links)) {
        hdf_rows <- which(grepl("\\.hdf$", download_links$href, ignore.case = TRUE))
        if (length(hdf_rows) > 0) {
          hdf_link <- download_links$href[hdf_rows[1]]
        }
      } else if (is.list(download_links)) {
        for (link in download_links) {
          if (grepl("\\.hdf$", link$href, ignore.case = TRUE)) {
            hdf_link <- link$href
            break
          }
        }
      }
      
      if (is.null(hdf_link)) {
        add_log_message(paste("No HDF download link found for:", granule_title))
        next
      }
      
      # Output filename
      output_file <- file.path(output_dir, paste0(granule_title, ".hdf"))
      
      # Skip if file already exists
      if (file.exists(output_file)) {
        add_log_message(paste("File already exists, skipping:", basename(output_file)))
        downloaded_files <- c(downloaded_files, output_file)
        next
      }
      
      # Download file with authentication
      tryCatch({
        # Authenticate with Earthdata
        auth_response <- httr::GET("https://urs.earthdata.nasa.gov/api/users/tokens",
                                  httr::authenticate(username, password))
        
        if (httr::status_code(auth_response) == 200) {
          # Download with authenticated session
          httr::GET(hdf_link,
                   httr::authenticate(username, password),
                   httr::progress(),
                   httr::write_disk(output_file, overwrite = TRUE),
                   httr::config(followlocation = TRUE))
          
          downloaded_files <- c(downloaded_files, output_file)
          add_log_message(paste("Successfully downloaded:", basename(output_file)))
          
          # Extract metadata
          metadata_list[[i]] <- data.frame(
            file = output_file,
            granule_id = granule_id,
            title = granule_title,
            product = product,
            sensing_date = extract_modis_date(granule_title),
            size_mb = file.size(output_file) / 1024^2,
            stringsAsFactors = FALSE
          )
        } else {
          add_log_message(paste("Authentication failed for:", granule_title))
        }
        
      }, error = function(e) {
        add_log_message(paste("Download failed for:", granule_title, "-", e$message))
      })
    }
  }
  
  # Combine metadata
  if (length(metadata_list) > 0) {
    metadata <- do.call(rbind, metadata_list)
  } else {
    metadata <- data.frame()
  }
  
  return(list(files = downloaded_files, metadata = metadata))
}

# Helper function for MODISTools downloads
download_with_modis_tools <- function(product, start_date, end_date, bbox,
                                    output_dir, max_files, quality_day,
                                    collection, add_log_message) {
  
  add_log_message("Using MODISTools for download...")
  
  if (!requireNamespace("MODISTools", quietly = TRUE)) {
    stop("MODISTools package required. Install with: install.packages('MODISTools')")
  }
  
  # MODISTools uses a different approach - it downloads data for specific locations
  # This is a simplified implementation
  add_log_message("MODISTools implementation requires specific lat/lon points")
  add_log_message("Converting bounding box to center point for demonstration")
  
  # Calculate center point
  center_lon <- mean(c(bbox[1], bbox[3]))
  center_lat <- mean(c(bbox[2], bbox[4]))
  
  add_log_message(paste("Using center point:", center_lat, ",", center_lon))
  
  # This would need to be expanded to handle the full bounding box
  # For now, return empty to indicate this needs full implementation
  return(list(files = character(), metadata = data.frame()))
}

# Helper functions for MODIS processing

get_modis_product_info <- function(product) {
  # Map MODIS products to CMR search parameters
  product_map <- list(
    "MOD04_L2" = list(short_name = "MOD04_L2", platform = "Terra"),
    "MYD04_L2" = list(short_name = "MYD04_L2", platform = "Aqua"),
    "MOD04_3K" = list(short_name = "MOD04_3K", platform = "Terra"),
    "MYD04_3K" = list(short_name = "MYD04_3K", platform = "Aqua"),
    "MOD14A1" = list(short_name = "MOD14A1", platform = "Terra"),
    "MYD14A1" = list(short_name = "MYD14A1", platform = "Aqua"),
    "MOD08_D3" = list(short_name = "MOD08_D3", platform = "Terra"),
    "MYD08_D3" = list(short_name = "MYD08_D3", platform = "Aqua"),
    "MOD08_M3" = list(short_name = "MOD08_M3", platform = "Terra"),
    "MYD08_M3" = list(short_name = "MYD08_M3", platform = "Aqua"),
    "MCD43A4" = list(short_name = "MCD43A4", platform = NULL)
  )
  
  return(product_map[[product]] %||% list(short_name = product, platform = NULL))
}

extract_modis_date <- function(filename) {
  # Extract date from MODIS filename (format: A2023001 = day 1 of 2023)
  date_match <- regmatches(filename, regexpr("A\\d{7}", filename))
  
  if (length(date_match) > 0) {
    # Parse MODIS date format
    year <- substr(date_match[1], 2, 5)
    doy <- substr(date_match[1], 6, 8)
    
    # Convert day of year to date
    base_date <- as.Date(paste0(year, "-01-01"))
    sensing_date <- base_date + as.numeric(doy) - 1
    
    return(sensing_date)
  }
  
  return(Sys.Date())
}

# Null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
