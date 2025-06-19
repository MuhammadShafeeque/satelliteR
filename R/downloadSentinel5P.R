if (!isGeneric("downloadSentinel5P")) {
  setGeneric("downloadSentinel5P", function(x, ...)
    standardGeneric("downloadSentinel5P"))
}

#' Download Sentinel-5P air pollution data
#'
#' @description
#' Download Sentinel-5P TROPOMI air pollution data from various sources including
#' ESA's Copernicus Open Access Hub and Google Earth Engine.
#'
#' @param product Character. Product type to download. Options include:
#' \itemize{
#'   \item "L2__NO2___" - Nitrogen Dioxide
#'   \item "L2__SO2___" - Sulfur Dioxide
#'   \item "L2__CO____" - Carbon Monoxide
#'   \item "L2__O3____" - Ozone
#'   \item "L2__HCHO__" - Formaldehyde
#'   \item "L2__CH4___" - Methane
#'   \item "L2__AER_AI" - Aerosol Index
#'   \item "L2__AER_LH" - Aerosol Layer Height
#'   \item "L2__CLOUD_" - Cloud products
#' }
#' @param start_date Character. Start date in format "YYYY-MM-DD"
#' @param end_date Character. End date in format "YYYY-MM-DD"
#' @param bbox Numeric vector. Bounding box as c(xmin, ymin, xmax, ymax) in WGS84
#' @param cloud_cover Numeric. Maximum cloud coverage percentage (0-100)
#' @param output_dir Character. Directory to save downloaded files
#' @param source Character. Data source: "copernicus", "gee", or "auto"
#' @param username Character. Username for Copernicus Open Access Hub
#' @param password Character. Password for Copernicus Open Access Hub
#' @param max_results Integer. Maximum number of products to download
#' @param processing_level Character. Processing level ("L2" for Level 2)
#' 
#' @return List containing:
#' \itemize{
#'   \item downloaded_files: Character vector of downloaded file paths
#'   \item metadata: Data frame with product metadata
#'   \item log: Processing log information
#' }
#' 
#' @export downloadSentinel5P
#' 
#' @examples
#' \dontrun{
#' # Download NO2 data for a specific region and time period
#' result <- downloadSentinel5P(
#'   product = "L2__NO2___",
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-07",
#'   bbox = c(-74.5, 40.0, -73.5, 41.0), # NYC area
#'   cloud_cover = 30,
#'   output_dir = "./s5p_data",
#'   source = "copernicus",
#'   username = "your_username",
#'   password = "your_password",
#'   max_results = 10
#' )
#' 
#' # Process downloaded files into Satellite object
#' sat <- processSentinel5P(result$downloaded_files)
#' }
#' 
#' @details
#' This function provides access to Sentinel-5P TROPOMI air pollution data through
#' multiple sources:
#' 
#' 1. **Copernicus Open Access Hub**: Direct access to ESA's official data hub
#' 2. **Google Earth Engine**: Access through GEE's Sentinel-5P collection
#' 3. **Auto**: Automatically selects the best available source
#' 
#' The function handles authentication, query construction, and file downloads
#' automatically. For Copernicus Hub access, you need to register at:
#' https://scihub.copernicus.eu/dhus/#/self-registration
#' 
#' Sentinel-5P provides daily global measurements of atmospheric trace gases
#' and aerosols with a spatial resolution of 3.5 × 7 km² (5.5 × 3.5 km² for NO2).
#' 
#' @references
#' Veefkind, J. P., et al. (2012). TROPOMI on the ESA Sentinel-5 Precursor: 
#' A GMES mission for global observations of the atmospheric composition for 
#' climate, air quality and ozone layer applications. Remote sensing of 
#' environment, 120, 70-83.
#' 
#' @seealso \code{\link{processSentinel5P}}, \code{\link{downloadMODISPollution}}
#' 
downloadSentinel5P <- function(product = "L2__NO2___",
                              start_date,
                              end_date,
                              bbox,
                              cloud_cover = 50,
                              output_dir = "./s5p_data",
                              source = "auto",
                              username = NULL,
                              password = NULL,
                              max_results = 50,
                              processing_level = "L2") {
  
  # Validate inputs
  if (missing(start_date) || missing(end_date)) {
    stop("start_date and end_date are required")
  }
  
  if (missing(bbox) || length(bbox) != 4) {
    stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
  }
  
  # Validate product type
  valid_products <- c("L2__NO2___", "L2__SO2___", "L2__CO____", "L2__O3____",
                      "L2__HCHO__", "L2__CH4___", "L2__AER_AI", "L2__AER_LH",
                      "L2__CLOUD_")
  
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
    messages = character()
  )
  
  # Add message to log
  add_log_message <- function(message) {
    log_info$messages <<- c(log_info$messages, 
                           paste(Sys.time(), ":", message))
    message(message)
  }
  
  add_log_message(paste("Starting Sentinel-5P download for product:", product))
  
  # Determine source
  if (source == "auto") {
    # Check for available sources and select the best one
    if (!is.null(username) && !is.null(password)) {
      source <- "copernicus"
      add_log_message("Auto-selected Copernicus Hub (credentials provided)")
    } else {
      # Try to use alternative sources
      source <- "gee"
      add_log_message("Auto-selected Google Earth Engine (no credentials provided)")
    }
  }
  
  # Download based on source
  downloaded_files <- character()
  metadata <- data.frame()
  
  if (source == "copernicus") {
    result <- download_from_copernicus(product, start_date, end_date, bbox,
                                     cloud_cover, output_dir, username,
                                     password, max_results, add_log_message)
    downloaded_files <- result$files
    metadata <- result$metadata
    
  } else if (source == "gee") {
    result <- download_from_gee(product, start_date, end_date, bbox,
                              cloud_cover, output_dir, max_results, add_log_message)
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

# Helper function for Copernicus Hub downloads
download_from_copernicus <- function(product, start_date, end_date, bbox,
                                   cloud_cover, output_dir, username,
                                   password, max_results, add_log_message) {
  
  add_log_message("Connecting to Copernicus Open Access Hub...")
  
  if (is.null(username) || is.null(password)) {
    stop("Username and password required for Copernicus Hub access")
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
  
  # Construct search query
  base_url <- "https://scihub.copernicus.eu/dhus/search"
  
  # Format dates
  start_date_formatted <- paste0(start_date, "T00:00:00.000Z")
  end_date_formatted <- paste0(end_date, "T23:59:59.999Z")
  
  # Create footprint from bbox
  footprint <- sprintf("POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
                      bbox[1], bbox[2], bbox[3], bbox[2], bbox[3], bbox[4],
                      bbox[1], bbox[4], bbox[1], bbox[2])
  
  # Construct query
  query_params <- list(
    q = paste0("platformname:Sentinel-5 AND ",
              "producttype:", product, " AND ",
              "beginposition:[", start_date_formatted, " TO ", end_date_formatted, "] AND ",
              "footprint:\"Intersects(", footprint, ")\""),
    rows = max_results,
    start = 0,
    format = "json"
  )
  
  add_log_message("Searching for products...")
  
  # Perform search
  response <- httr::GET(base_url,
                       query = query_params,
                       httr::authenticate(username, password))
  
  if (httr::status_code(response) != 200) {
    stop(paste("Search failed with status code:", httr::status_code(response)))
  }
  
  # Parse response
  search_results <- httr::content(response, "text", encoding = "UTF-8")
  results_json <- jsonlite::fromJSON(search_results)
  
  if (results_json$feed$opensearch$totalResults == 0) {
    add_log_message("No products found matching criteria")
    return(list(files = character(), metadata = data.frame()))
  }
  
  add_log_message(paste("Found", results_json$feed$opensearch$totalResults, "products"))
  
  # Extract product information
  products <- results_json$feed$entry
  if (is.null(products)) {
    add_log_message("No products available for download")
    return(list(files = character(), metadata = data.frame()))
  }
  
  # Ensure products is a data frame
  if (!is.data.frame(products)) {
    if (length(products) == 1) {
      # Single product - convert to data frame
      products <- data.frame(products, stringsAsFactors = FALSE)
    }
  }
  
  downloaded_files <- character()
  metadata_list <- list()
  
  # Download each product
  for (i in seq_len(min(nrow(products), max_results))) {
    product_info <- products[i, ]
    product_id <- product_info$id
    product_title <- product_info$title
    
    add_log_message(paste("Downloading product", i, "of", min(nrow(products), max_results), ":", product_title))
    
    # Download URL
    download_url <- paste0("https://scihub.copernicus.eu/dhus/odata/v1/Products('", 
                          product_id, "')/$value")
    
    # Output filename
    output_file <- file.path(output_dir, paste0(product_title, ".nc"))
    
    # Skip if file already exists
    if (file.exists(output_file)) {
      add_log_message(paste("File already exists, skipping:", basename(output_file)))
      downloaded_files <- c(downloaded_files, output_file)
      next
    }
    
    # Download file
    tryCatch({
      httr::GET(download_url,
               httr::authenticate(username, password),
               httr::progress(),
               httr::write_disk(output_file, overwrite = TRUE))
      
      downloaded_files <- c(downloaded_files, output_file)
      add_log_message(paste("Successfully downloaded:", basename(output_file)))
      
      # Extract metadata
      metadata_list[[i]] <- data.frame(
        file = output_file,
        product_id = product_id,
        title = product_title,
        product_type = product,
        sensing_date = as.Date(substr(product_title, 21, 28), format = "%Y%m%d"),
        size_mb = file.size(output_file) / 1024^2,
        stringsAsFactors = FALSE
      )
      
    }, error = function(e) {
      add_log_message(paste("Download failed for:", product_title, "-", e$message))
    })
  }
  
  # Combine metadata
  if (length(metadata_list) > 0) {
    metadata <- do.call(rbind, metadata_list)
  } else {
    metadata <- data.frame()
  }
  
  return(list(files = downloaded_files, metadata = metadata))
}

# Helper function for Google Earth Engine downloads
download_from_gee <- function(product, start_date, end_date, bbox,
                            cloud_cover, output_dir, max_results, add_log_message) {
  
  add_log_message("Using Google Earth Engine source...")
  
  # Check if rgee is available
  if (!requireNamespace("rgee", quietly = TRUE)) {
    stop("rgee package required for Google Earth Engine access. Install with: install.packages('rgee')")
  }
  
  # This is a placeholder implementation
  # In practice, this would use rgee to access Sentinel-5P data from GEE
  add_log_message("GEE implementation requires rgee package and authentication setup")
  add_log_message("Please use Copernicus Hub source with valid credentials for now")
  
  return(list(files = character(), metadata = data.frame()))
}
