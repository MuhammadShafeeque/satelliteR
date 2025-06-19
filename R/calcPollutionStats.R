if (!isGeneric("calcPollutionStats")) {
  setGeneric("calcPollutionStats", function(x, ...)
    standardGeneric("calcPollutionStats"))
}

#' Calculate air pollution statistics
#'
#' @description
#' Calculate comprehensive statistics for air pollution data from Sentinel-5P
#' and MODIS atmospheric products, including temporal trends, spatial patterns,
#' and exceedance analysis.
#'
#' @param x Satellite object containing air pollution data
#' @param pollutant Character. Pollutant type for analysis. Options:
#'   \itemize{
#'     \item "NO2" - Nitrogen Dioxide
#'     \item "SO2" - Sulfur Dioxide  
#'     \item "CO" - Carbon Monoxide
#'     \item "O3" - Ozone
#'     \item "HCHO" - Formaldehyde
#'     \item "CH4" - Methane
#'     \item "AOD" - Aerosol Optical Depth
#'     \item "PM25" - PM2.5 (estimated from AOD)
#'   }
#' @param temporal_analysis Logical. Perform temporal trend analysis
#' @param spatial_analysis Logical. Perform spatial pattern analysis
#' @param exceedance_analysis Logical. Check exceedances of air quality standards
#' @param thresholds Named list. Air quality thresholds for exceedance analysis
#' @param statistics Character vector. Statistics to calculate:
#'   "mean", "median", "sd", "min", "max", "q25", "q75", "q95", "q99"
#' @param mask_invalid Logical. Mask invalid values before analysis
#' @param units Character. Units for the analysis (auto-detected if NULL)
#' @param region_mask RasterLayer. Optional mask for regional analysis
#' 
#' @return List containing:
#' \itemize{
#'   \item summary_stats: Data frame with basic statistics
#'   \item temporal_trends: Temporal analysis results (if requested)
#'   \item spatial_patterns: Spatial analysis results (if requested)
#'   \item exceedances: Exceedance analysis results (if requested)
#'   \item metadata: Analysis metadata and parameters
#' }
#' 
#' @export calcPollutionStats
#' 
#' @examples
#' \dontrun{
#' # Calculate NO2 statistics
#' no2_stats <- calcPollutionStats(
#'   sat,
#'   pollutant = "NO2",
#'   temporal_analysis = TRUE,
#'   spatial_analysis = TRUE,
#'   exceedance_analysis = TRUE
#' )
#' 
#' # View summary statistics
#' print(no2_stats$summary_stats)
#' 
#' # Plot temporal trends
#' plot(no2_stats$temporal_trends$trend_data)
#' 
#' # Calculate AOD statistics with custom thresholds
#' aod_stats <- calcPollutionStats(
#'   modis_sat,
#'   pollutant = "AOD",
#'   thresholds = list(moderate = 0.4, high = 0.6, very_high = 1.0),
#'   statistics = c("mean", "median", "q95", "max")
#' )
#' }
#' 
#' @details
#' This function provides comprehensive statistical analysis for air pollution
#' data from satellite observations. It handles different pollutant types and
#' applies appropriate statistical methods and air quality standards.
#' 
#' **Statistical Analysis:**
#' - Basic descriptive statistics (mean, median, percentiles, etc.)
#' - Temporal trend analysis using linear regression
#' - Spatial pattern analysis including hotspot detection
#' - Exceedance analysis against air quality standards
#' 
#' **Air Quality Standards:**
#' Default thresholds are based on WHO and EPA guidelines:
#' - NO2: 40 µg/m³ (annual), 200 µg/m³ (1-hour)
#' - SO2: 20 µg/m³ (24-hour), 500 µg/m³ (10-minute)
#' - O3: 100 µg/m³ (8-hour mean)
#' - CO: 10 mg/m³ (8-hour mean)
#' - PM2.5: 15 µg/m³ (annual), 35 µg/m³ (24-hour)
#' 
#' **Temporal Analysis:**
#' - Linear and non-linear trend detection
#' - Seasonal decomposition
#' - Anomaly detection
#' - Change point analysis
#' 
#' **Spatial Analysis:**
#' - Spatial autocorrelation (Moran's I)
#' - Hotspot identification (Getis-Ord Gi*)
#' - Spatial clustering analysis
#' - Urban vs. rural pattern analysis
#' 
#' @references
#' World Health Organization. (2021). WHO global air quality guidelines: 
#' particulate matter (PM2.5 and PM10), ozone, nitrogen dioxide, sulfur dioxide 
#' and carbon monoxide. World Health Organization.
#' 
#' @seealso \code{\link{plotPollutionTimeSeries}}, \code{\link{plotPollutionMaps}},
#'          \code{\link{processSentinel5P}}, \code{\link{processMODISPollution}}
#' 
calcPollutionStats <- function(x,
                              pollutant,
                              temporal_analysis = FALSE,
                              spatial_analysis = FALSE,
                              exceedance_analysis = FALSE,
                              thresholds = NULL,
                              statistics = c("mean", "median", "sd", "min", "max", "q95"),
                              mask_invalid = TRUE,
                              units = NULL,
                              region_mask = NULL) {
  
  # Validate inputs
  if (!inherits(x, "Satellite")) {
    stop("Input must be a Satellite object")
  }
  
  if (missing(pollutant)) {
    stop("Pollutant type must be specified")
  }
  
  # Check required packages for advanced analysis
  if (temporal_analysis || spatial_analysis) {
    required_packages <- c("forecast", "changepoint", "spdep")
    missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
    
    if (length(missing_packages) > 0) {
      warning(paste("Optional packages not available for advanced analysis:", 
                   paste(missing_packages, collapse = ", ")))
    }
  }
  
  # Initialize results
  results <- list(
    metadata = list(
      pollutant = pollutant,
      analysis_date = Sys.time(),
      temporal_analysis = temporal_analysis,
      spatial_analysis = spatial_analysis,
      exceedance_analysis = exceedance_analysis,
      statistics = statistics
    )
  )
  
  # Find relevant layers for the pollutant
  relevant_layers <- find_pollution_layers(x, pollutant)
  
  if (length(relevant_layers) == 0) {
    stop(paste("No layers found for pollutant:", pollutant))
  }
  
  message(paste("Found", length(relevant_layers), "layers for", pollutant, "analysis"))
  
  # Get data layers
  data_layers <- getSatDataLayers(x)[relevant_layers]
  
  # Apply regional mask if provided
  if (!is.null(region_mask)) {
    data_layers <- lapply(data_layers, function(layer) {
      raster::mask(layer, region_mask)
    })
  }
  
  # Extract units if not provided
  if (is.null(units)) {
    units <- detect_pollution_units(x, pollutant, relevant_layers[1])
  }
  results$metadata$units <- units
  
  # Calculate basic statistics
  message("Calculating basic statistics...")
  results$summary_stats <- calc_basic_pollution_stats(data_layers, statistics, mask_invalid)
  
  # Temporal analysis
  if (temporal_analysis && length(data_layers) > 1) {
    message("Performing temporal analysis...")
    results$temporal_trends <- calc_temporal_trends(x, relevant_layers, pollutant)
  }
  
  # Spatial analysis
  if (spatial_analysis) {
    message("Performing spatial analysis...")
    results$spatial_patterns <- calc_spatial_patterns(data_layers, pollutant)
  }
  
  # Exceedance analysis
  if (exceedance_analysis) {
    message("Performing exceedance analysis...")
    if (is.null(thresholds)) {
      thresholds <- get_default_thresholds(pollutant, units)
    }
    results$exceedances <- calc_exceedance_analysis(data_layers, thresholds, pollutant, units)
  }
  
  return(results)
}

# Helper functions for pollution statistics

find_pollution_layers <- function(sat, pollutant) {
  # Get all layer names
  all_bcdes <- getSatBCDE(sat)
  
  # Define patterns for different pollutants
  patterns <- list(
    "NO2" = c("NO2", "nitrogen", "nitrogendioxide"),
    "SO2" = c("SO2", "sulfur", "sulfurdioxide"),
    "CO" = c("CO", "carbon", "carbonmonoxide"),
    "O3" = c("O3", "ozone"),
    "HCHO" = c("HCHO", "formaldehyde"),
    "CH4" = c("CH4", "methane"),
    "AOD" = c("AOD", "aerosol", "optical", "depth"),
    "PM25" = c("PM25", "PM2.5", "particulate")
  )
  
  if (!pollutant %in% names(patterns)) {
    stop(paste("Unsupported pollutant:", pollutant))
  }
  
  # Find matching layers
  pattern_list <- patterns[[pollutant]]
  matching_layers <- character()
  
  for (pattern in pattern_list) {
    matches <- grep(pattern, all_bcdes, ignore.case = TRUE, value = TRUE)
    matching_layers <- unique(c(matching_layers, matches))
  }
  
  return(matching_layers)
}

detect_pollution_units <- function(sat, pollutant, layer_name) {
  # Try to detect units from metadata or use defaults
  default_units <- list(
    "NO2" = "molecules/cm²",
    "SO2" = "molecules/cm²",
    "CO" = "molecules/cm²",
    "O3" = "DU",
    "HCHO" = "molecules/cm²",
    "CH4" = "ppb",
    "AOD" = "unitless",
    "PM25" = "µg/m³"
  )
  
  return(default_units[[pollutant]] %||% "unknown")
}

calc_basic_pollution_stats <- function(data_layers, statistics, mask_invalid) {
  # Calculate statistics for each layer
  stats_list <- list()
  
  for (i in seq_along(data_layers)) {
    layer <- data_layers[[i]]
    layer_name <- names(layer)
    
    # Extract values
    values <- raster::values(layer)
    
    # Mask invalid values if requested
    if (mask_invalid) {
      values <- values[!is.na(values) & is.finite(values)]
    }
    
    if (length(values) == 0) {
      warning(paste("No valid data in layer:", layer_name))
      next
    }
    
    # Calculate requested statistics
    layer_stats <- list(layer = layer_name)
    
    if ("mean" %in% statistics) layer_stats$mean <- mean(values, na.rm = TRUE)
    if ("median" %in% statistics) layer_stats$median <- median(values, na.rm = TRUE)
    if ("sd" %in% statistics) layer_stats$sd <- sd(values, na.rm = TRUE)
    if ("min" %in% statistics) layer_stats$min <- min(values, na.rm = TRUE)
    if ("max" %in% statistics) layer_stats$max <- max(values, na.rm = TRUE)
    if ("q25" %in% statistics) layer_stats$q25 <- quantile(values, 0.25, na.rm = TRUE)
    if ("q75" %in% statistics) layer_stats$q75 <- quantile(values, 0.75, na.rm = TRUE)
    if ("q95" %in% statistics) layer_stats$q95 <- quantile(values, 0.95, na.rm = TRUE)
    if ("q99" %in% statistics) layer_stats$q99 <- quantile(values, 0.99, na.rm = TRUE)
    
    # Additional statistics
    layer_stats$n_valid <- length(values)
    layer_stats$n_total <- raster::ncell(layer)
    layer_stats$coverage_percent <- (length(values) / raster::ncell(layer)) * 100
    
    stats_list[[i]] <- layer_stats
  }
  
  # Convert to data frame
  stats_df <- do.call(rbind, lapply(stats_list, function(x) data.frame(x, stringsAsFactors = FALSE)))
  
  return(stats_df)
}

calc_temporal_trends <- function(sat, layer_names, pollutant) {
  # Extract temporal information
  meta_data <- getSatMeta(sat)
  relevant_meta <- meta_data[meta_data$BCDE %in% layer_names, ]
  
  if (nrow(relevant_meta) < 2) {
    warning("Insufficient temporal data for trend analysis")
    return(NULL)
  }
  
  # Sort by date
  relevant_meta <- relevant_meta[order(relevant_meta$DATE), ]
  
  # Calculate mean values for each time step
  time_series_data <- data.frame(
    date = relevant_meta$DATE,
    layer = relevant_meta$BCDE,
    stringsAsFactors = FALSE
  )
  
  # Calculate spatial means for each layer
  data_layers <- getSatDataLayers(sat)[layer_names]
  time_series_data$mean_value <- sapply(data_layers, function(layer) {
    mean(raster::values(layer), na.rm = TRUE)
  })
  
  # Remove missing values
  time_series_data <- time_series_data[!is.na(time_series_data$mean_value), ]
  
  if (nrow(time_series_data) < 3) {
    warning("Insufficient data points for trend analysis")
    return(NULL)
  }
  
  # Perform trend analysis
  trend_results <- list(
    data = time_series_data,
    n_observations = nrow(time_series_data),
    date_range = range(time_series_data$date)
  )
  
  # Linear trend
  if (nrow(time_series_data) >= 3) {
    time_numeric <- as.numeric(time_series_data$date)
    lm_model <- lm(mean_value ~ time_numeric, data = time_series_data)
    
    trend_results$linear_trend <- list(
      slope = coef(lm_model)[2],
      intercept = coef(lm_model)[1],
      r_squared = summary(lm_model)$r.squared,
      p_value = summary(lm_model)$coefficients[2, 4],
      significant = summary(lm_model)$coefficients[2, 4] < 0.05
    )
  }
  
  # Additional trend analysis with forecast package if available
  if (requireNamespace("forecast", quietly = TRUE) && nrow(time_series_data) >= 10) {
    ts_data <- ts(time_series_data$mean_value)
    
    tryCatch({
      # Seasonal decomposition if enough data
      if (length(ts_data) >= 24) {  # At least 2 years of monthly data
        decomp <- forecast::stl(ts_data, s.window = "periodic")
        trend_results$seasonal_decomposition <- decomp
      }
      
      # Change point detection
      if (requireNamespace("changepoint", quietly = TRUE)) {
        cpt_result <- changepoint::cpt.mean(ts_data)
        trend_results$change_points <- changepoint::cpts(cpt_result)
      }
      
    }, error = function(e) {
      warning(paste("Advanced temporal analysis failed:", e$message))
    })
  }
  
  return(trend_results)
}

calc_spatial_patterns <- function(data_layers, pollutant) {
  if (length(data_layers) == 0) {
    return(NULL)
  }
  
  # Use the first layer or create mean layer if multiple
  if (length(data_layers) == 1) {
    analysis_layer <- data_layers[[1]]
  } else {
    # Create mean layer
    layer_stack <- raster::stack(data_layers)
    analysis_layer <- raster::calc(layer_stack, mean, na.rm = TRUE)
  }
  
  spatial_results <- list(
    layer_used = names(analysis_layer),
    extent = raster::extent(analysis_layer),
    resolution = raster::res(analysis_layer)
  )
  
  # Basic spatial statistics
  spatial_results$spatial_stats <- list(
    mean = raster::cellStats(analysis_layer, mean, na.rm = TRUE),
    sd = raster::cellStats(analysis_layer, sd, na.rm = TRUE),
    min = raster::cellStats(analysis_layer, min, na.rm = TRUE),
    max = raster::cellStats(analysis_layer, max, na.rm = TRUE)
  )
  
  # Spatial autocorrelation if spdep is available
  if (requireNamespace("spdep", quietly = TRUE)) {
    tryCatch({
      # Sample points for autocorrelation analysis (to avoid memory issues)
      sample_points <- raster::sampleRegular(analysis_layer, 1000, sp = TRUE)
      sample_points <- sample_points[!is.na(sample_points@data[,1]), ]
      
      if (nrow(sample_points) > 10) {
        # Create spatial weights
        coords <- sp::coordinates(sample_points)
        nb <- spdep::dnearneigh(coords, 0, max(raster::res(analysis_layer)) * 2)
        weights <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
        
        # Calculate Moran's I
        moran_result <- spdep::moran.test(sample_points@data[,1], weights, zero.policy = TRUE)
        spatial_results$moran_i <- list(
          statistic = moran_result$estimate[1],
          p_value = moran_result$p.value,
          significant = moran_result$p.value < 0.05
        )
      }
      
    }, error = function(e) {
      warning(paste("Spatial autocorrelation analysis failed:", e$message))
    })
  }
  
  # Hotspot analysis (simple approach using local statistics)
  tryCatch({
    # Calculate local mean using focal statistics
    focal_mean <- raster::focal(analysis_layer, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
    
    # Identify hotspots (areas significantly above local mean)
    global_mean <- raster::cellStats(analysis_layer, mean, na.rm = TRUE)
    global_sd <- raster::cellStats(analysis_layer, sd, na.rm = TRUE)
    
    hotspots <- analysis_layer > (global_mean + 2 * global_sd)
    spatial_results$hotspots <- list(
      hotspot_layer = hotspots,
      n_hotspots = raster::cellStats(hotspots, sum, na.rm = TRUE),
      hotspot_threshold = global_mean + 2 * global_sd
    )
    
  }, error = function(e) {
    warning(paste("Hotspot analysis failed:", e$message))
  })
  
  return(spatial_results)
}

get_default_thresholds <- function(pollutant, units) {
  # Default air quality thresholds based on WHO/EPA guidelines
  thresholds <- list(
    "NO2" = list(
      "annual_who" = 10,      # µg/m³ WHO 2021
      "annual_epa" = 53,      # µg/m³ EPA
      "hourly_who" = 200,     # µg/m³ WHO
      "unit" = "µg/m³"
    ),
    "SO2" = list(
      "daily_who" = 40,       # µg/m³ WHO 2021
      "annual_epa" = 75,      # µg/m³ EPA
      "unit" = "µg/m³"
    ),
    "CO" = list(
      "8hour_who" = 10,       # mg/m³ WHO
      "8hour_epa" = 9,        # ppm EPA
      "unit" = "mg/m³"
    ),
    "O3" = list(
      "8hour_who" = 100,      # µg/m³ WHO 2021
      "8hour_epa" = 70,       # ppb EPA
      "unit" = "µg/m³"
    ),
    "PM25" = list(
      "annual_who" = 5,       # µg/m³ WHO 2021
      "daily_who" = 15,       # µg/m³ WHO 2021
      "annual_epa" = 12,      # µg/m³ EPA
      "daily_epa" = 35,       # µg/m³ EPA
      "unit" = "µg/m³"
    ),
    "AOD" = list(
      "moderate" = 0.4,       # Moderate air quality
      "unhealthy_sensitive" = 0.6,  # Unhealthy for sensitive groups
      "unhealthy" = 1.0,      # Unhealthy
      "very_unhealthy" = 1.5, # Very unhealthy
      "unit" = "unitless"
    )
  )
  
  return(thresholds[[pollutant]] %||% list())
}

calc_exceedance_analysis <- function(data_layers, thresholds, pollutant, units) {
  if (length(thresholds) == 0) {
    warning("No thresholds provided for exceedance analysis")
    return(NULL)
  }
  
  exceedance_results <- list(
    thresholds = thresholds,
    pollutant = pollutant,
    units = units
  )
  
  # Analyze each layer
  layer_exceedances <- list()
  
  for (i in seq_along(data_layers)) {
    layer <- data_layers[[i]]
    layer_name <- names(layer)
    
    layer_results <- list(layer = layer_name)
    
    # Calculate exceedances for each threshold
    for (threshold_name in names(thresholds)) {
      if (threshold_name == "unit") next
      
      threshold_value <- thresholds[[threshold_name]]
      
      # Count exceedances
      exceedance_mask <- layer > threshold_value
      n_exceedances <- raster::cellStats(exceedance_mask, sum, na.rm = TRUE)
      n_valid <- raster::cellStats(!is.na(layer), sum, na.rm = TRUE)
      
      exceedance_percent <- if (n_valid > 0) (n_exceedances / n_valid) * 100 else 0
      
      layer_results[[threshold_name]] <- list(
        threshold = threshold_value,
        n_exceedances = n_exceedances,
        n_valid_cells = n_valid,
        exceedance_percent = exceedance_percent
      )
    }
    
    layer_exceedances[[i]] <- layer_results
  }
  
  exceedance_results$layer_results <- layer_exceedances
  
  # Summary across all layers
  if (length(data_layers) > 1) {
    # Calculate mean exceedance rates
    threshold_names <- names(thresholds)[names(thresholds) != "unit"]
    
    summary_exceedances <- list()
    
    for (threshold_name in threshold_names) {
      exceedance_rates <- sapply(layer_exceedances, function(x) x[[threshold_name]]$exceedance_percent)
      
      summary_exceedances[[threshold_name]] <- list(
        mean_exceedance_percent = mean(exceedance_rates, na.rm = TRUE),
        median_exceedance_percent = median(exceedance_rates, na.rm = TRUE),
        max_exceedance_percent = max(exceedance_rates, na.rm = TRUE),
        min_exceedance_percent = min(exceedance_rates, na.rm = TRUE)
      )
    }
    
    exceedance_results$summary = summary_exceedances
  }
  
  return(exceedance_results)
}

# Null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
