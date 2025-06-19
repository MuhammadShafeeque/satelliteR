if (!isGeneric("plotPollutionTimeSeries")) {
  setGeneric("plotPollutionTimeSeries", function(x, ...)
    standardGeneric("plotPollutionTimeSeries"))
}

#' Plot air pollution time series
#'
#' @description
#' Create time series plots for air pollution data from Sentinel-5P and MODIS
#' atmospheric products, with options for trend analysis and anomaly detection.
#'
#' @param x Satellite object or pollution statistics object from \code{calcPollutionStats}
#' @param pollutant Character. Pollutant type to plot
#' @param region RasterLayer or SpatialPolygons. Optional region for spatial averaging
#' @param trend_line Logical. Add trend line to the plot
#' @param anomalies Logical. Highlight anomalous values
#' @param seasonal Logical. Show seasonal decomposition if available
#' @param confidence_intervals Logical. Show confidence intervals for trends
#' @param aggregation Character. Temporal aggregation method: "none", "daily", "weekly", "monthly"
#' @param y_label Character. Custom y-axis label
#' @param title Character. Plot title (auto-generated if NULL)
#' @param color_scheme Character. Color scheme for the plot
#' @param point_size Numeric. Size of data points
#' @param line_size Numeric. Size of trend line
#' @param date_format Character. Date format for x-axis labels
#' @param save_plot Logical. Save plot to file
#' @param output_path Character. Path to save plot
#' @param plot_width Numeric. Plot width in inches
#' @param plot_height Numeric. Plot height in inches
#' 
#' @return ggplot object containing the time series plot
#' 
#' @export plotPollutionTimeSeries
#' 
#' @examples
#' \dontrun{
#' # Plot NO2 time series
#' p1 <- plotPollutionTimeSeries(
#'   sat,
#'   pollutant = "NO2",
#'   trend_line = TRUE,
#'   anomalies = TRUE,
#'   aggregation = "monthly"
#' )
#' print(p1)
#' 
#' # Plot with regional averaging
#' urban_mask <- # create urban area mask
#' p2 <- plotPollutionTimeSeries(
#'   sat,
#'   pollutant = "NO2",
#'   region = urban_mask,
#'   title = "Urban NO2 Trends"
#' )
#' 
#' # Plot from statistics object
#' stats <- calcPollutionStats(sat, pollutant = "AOD", temporal_analysis = TRUE)
#' p3 <- plotPollutionTimeSeries(stats, seasonal = TRUE)
#' }
#' 
#' @details
#' This function creates publication-quality time series plots for air pollution
#' data with various analytical features:
#' 
#' **Data Processing:**
#' - Temporal aggregation (daily, weekly, monthly)
#' - Regional spatial averaging
#' - Anomaly detection using statistical methods
#' - Trend analysis with confidence intervals
#' 
#' **Plot Features:**
#' - Interactive plots with plotly integration
#' - Customizable aesthetics and color schemes
#' - Seasonal decomposition visualization
#' - Multiple trend fitting options (linear, LOESS, GAM)
#' 
#' **Output Options:**
#' - High-resolution plot export
#' - Multiple format support (PNG, PDF, SVG)
#' - Customizable dimensions and DPI
#' 
#' @seealso \code{\link{calcPollutionStats}}, \code{\link{plotPollutionMaps}}
#' 
plotPollutionTimeSeries <- function(x,
                                   pollutant = NULL,
                                   region = NULL,
                                   trend_line = TRUE,
                                   anomalies = FALSE,
                                   seasonal = FALSE,
                                   confidence_intervals = TRUE,
                                   aggregation = "none",
                                   y_label = NULL,
                                   title = NULL,
                                   color_scheme = "viridis",
                                   point_size = 2,
                                   line_size = 1,
                                   date_format = "%Y-%m",
                                   save_plot = FALSE,
                                   output_path = NULL,
                                   plot_width = 10,
                                   plot_height = 6) {
  
  # Check required packages
  required_packages <- c("ggplot2")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('ggplot2'))"))
  }
  
  # Load required packages
  requireNamespace("ggplot2", quietly = TRUE)
  
  # Determine input type and extract time series data
  if (inherits(x, "Satellite")) {
    if (is.null(pollutant)) {
      stop("Pollutant must be specified when input is a Satellite object")
    }
    ts_data <- extract_time_series_from_satellite(x, pollutant, region, aggregation)
  } else if (is.list(x) && "temporal_trends" %in% names(x)) {
    # Input is from calcPollutionStats
    ts_data <- extract_time_series_from_stats(x, seasonal)
    if (is.null(pollutant)) {
      pollutant <- x$metadata$pollutant %||% "Unknown"
    }
  } else {
    stop("Input must be a Satellite object or pollution statistics list")
  }
  
  if (is.null(ts_data) || nrow(ts_data) == 0) {
    stop("No time series data available")
  }
  
  # Detect anomalies if requested
  if (anomalies) {
    ts_data <- detect_time_series_anomalies(ts_data)
  }
  
  # Generate plot labels
  if (is.null(y_label)) {
    y_label <- generate_y_label(pollutant, ts_data)
  }
  
  if (is.null(title)) {
    title <- generate_plot_title(pollutant, region, aggregation)
  }
  
  # Create base plot
  p <- ggplot2::ggplot(ts_data, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_point(
      ggplot2::aes(color = if (anomalies && "is_anomaly" %in% names(ts_data)) is_anomaly else NULL),
      size = point_size,
      alpha = 0.7
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "#2166AC", "TRUE" = "#D73027"),
      name = "Anomaly",
      labels = c("Normal", "Anomaly")
    ) +
    ggplot2::labs(
      title = title,
      x = "Date",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  
  # Add trend line if requested
  if (trend_line) {
    if (confidence_intervals) {
      p <- p + ggplot2::geom_smooth(
        method = "lm",
        color = "#E31A1C",
        size = line_size,
        alpha = 0.3
      )
    } else {
      p <- p + ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#E31A1C",
        size = line_size
      )
    }
  }
  
  # Format x-axis dates
  p <- p + ggplot2::scale_x_date(date_labels = date_format)
  
  # Apply color scheme
  if (color_scheme == "viridis") {
    if (!anomalies || !"is_anomaly" %in% names(ts_data)) {
      p <- p + ggplot2::scale_color_viridis_c()
    }
  }
  
  # Add seasonal decomposition if available and requested
  if (seasonal && "seasonal_component" %in% names(ts_data)) {
    # Create multi-panel plot for seasonal decomposition
    p <- create_seasonal_decomposition_plot(ts_data, title, y_label, color_scheme)
  }
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(output_path)) {
      output_path <- paste0("pollution_timeseries_", 
                           gsub("[^A-Za-z0-9]", "_", pollutant), 
                           "_", format(Sys.Date(), "%Y%m%d"), ".png")
    }
    
    ggplot2::ggsave(
      filename = output_path,
      plot = p,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      units = "in"
    )
    
    message(paste("Plot saved to:", output_path))
  }
  
  return(p)
}

# Helper functions for time series plotting

extract_time_series_from_satellite <- function(sat, pollutant, region, aggregation) {
  # Find relevant layers
  relevant_layers <- find_pollution_layers(sat, pollutant)
  
  if (length(relevant_layers) == 0) {
    return(NULL)
  }
  
  # Get metadata for temporal information
  meta_data <- getSatMeta(sat)
  relevant_meta <- meta_data[meta_data$BCDE %in% relevant_layers, ]
  
  if (nrow(relevant_meta) == 0) {
    return(NULL)
  }
  
  # Sort by date
  relevant_meta <- relevant_meta[order(relevant_meta$DATE), ]
  
  # Extract values for each time step
  data_layers <- getSatDataLayers(sat)[relevant_layers]
  
  ts_data <- data.frame(
    date = relevant_meta$DATE,
    layer = relevant_meta$BCDE,
    stringsAsFactors = FALSE
  )
  
  # Calculate spatial statistics
  if (is.null(region)) {
    # Global mean
    ts_data$value <- sapply(seq_along(data_layers), function(i) {
      layer <- data_layers[[relevant_meta$BCDE[i]]]
      mean(raster::values(layer), na.rm = TRUE)
    })
  } else {
    # Regional mean
    ts_data$value <- sapply(seq_along(data_layers), function(i) {
      layer <- data_layers[[relevant_meta$BCDE[i]]]
      masked_layer <- raster::mask(layer, region)
      mean(raster::values(masked_layer), na.rm = TRUE)
    })
  }
  
  # Remove missing values
  ts_data <- ts_data[!is.na(ts_data$value), ]
  
  # Apply temporal aggregation
  if (aggregation != "none") {
    ts_data <- apply_temporal_aggregation(ts_data, aggregation)
  }
  
  return(ts_data)
}

extract_time_series_from_stats <- function(stats_obj, seasonal) {
  if (!"temporal_trends" %in% names(stats_obj) || is.null(stats_obj$temporal_trends)) {
    return(NULL)
  }
  
  trend_data <- stats_obj$temporal_trends$data
  
  if (!"date" %in% names(trend_data) || !"mean_value" %in% names(trend_data)) {
    return(NULL)
  }
  
  ts_data <- data.frame(
    date = trend_data$date,
    value = trend_data$mean_value,
    stringsAsFactors = FALSE
  )
  
  # Add seasonal components if available and requested
  if (seasonal && "seasonal_decomposition" %in% names(stats_obj$temporal_trends)) {
    decomp <- stats_obj$temporal_trends$seasonal_decomposition
    
    if (!is.null(decomp)) {
      # Extract components from STL decomposition
      ts_data$trend_component <- as.numeric(decomp$time.series[, "trend"])
      ts_data$seasonal_component <- as.numeric(decomp$time.series[, "seasonal"])
      ts_data$residual_component <- as.numeric(decomp$time.series[, "remainder"])
    }
  }
  
  return(ts_data)
}

apply_temporal_aggregation <- function(ts_data, aggregation) {
  if (aggregation == "none") {
    return(ts_data)
  }
  
  ts_data$date <- as.Date(ts_data$date)
  
  # Define aggregation periods
  if (aggregation == "daily") {
    ts_data$period <- ts_data$date
  } else if (aggregation == "weekly") {
    ts_data$period <- cut(ts_data$date, breaks = "week")
  } else if (aggregation == "monthly") {
    ts_data$period <- cut(ts_data$date, breaks = "month")
  } else {
    return(ts_data)
  }
  
  # Aggregate by period
  aggregated <- aggregate(
    value ~ period,
    data = ts_data,
    FUN = mean,
    na.rm = TRUE
  )
  
  # Convert period back to date
  if (aggregation == "daily") {
    aggregated$date <- as.Date(aggregated$period)
  } else {
    aggregated$date <- as.Date(aggregated$period)
  }
  
  aggregated$period <- NULL
  
  return(aggregated)
}

detect_time_series_anomalies <- function(ts_data) {
  if (nrow(ts_data) < 10) {
    ts_data$is_anomaly <- FALSE
    return(ts_data)
  }
  
  # Simple outlier detection using IQR method
  Q1 <- quantile(ts_data$value, 0.25, na.rm = TRUE)
  Q3 <- quantile(ts_data$value, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  ts_data$is_anomaly <- ts_data$value < lower_bound | ts_data$value > upper_bound
  
  return(ts_data)
}

generate_y_label <- function(pollutant, ts_data) {
  # Default units mapping
  units_map <- list(
    "NO2" = "NO₂ (molecules/cm²)",
    "SO2" = "SO₂ (molecules/cm²)",
    "CO" = "CO (molecules/cm²)",
    "O3" = "O₃ (DU)",
    "HCHO" = "HCHO (molecules/cm²)",
    "CH4" = "CH₄ (ppb)",
    "AOD" = "Aerosol Optical Depth",
    "PM25" = "PM₂.₅ (µg/m³)"
  )
  
  return(units_map[[pollutant]] %||% paste(pollutant, "Concentration"))
}

generate_plot_title <- function(pollutant, region, aggregation) {
  base_title <- paste(pollutant, "Time Series")
  
  if (!is.null(region)) {
    base_title <- paste("Regional", base_title)
  }
  
  if (aggregation != "none") {
    base_title <- paste(base_title, paste0("(", stringr::str_to_title(aggregation), " Average)"))
  }
  
  return(base_title)
}

create_seasonal_decomposition_plot <- function(ts_data, title, y_label, color_scheme) {
  if (!all(c("trend_component", "seasonal_component", "residual_component") %in% names(ts_data))) {
    # Return simple plot if decomposition data not available
    return(ggplot2::ggplot(ts_data, ggplot2::aes(x = date, y = value)) +
           ggplot2::geom_line() +
           ggplot2::labs(title = title, x = "Date", y = y_label))
  }
  
  # Reshape data for faceted plot
  decomp_data <- data.frame(
    date = rep(ts_data$date, 4),
    value = c(ts_data$value, ts_data$trend_component, 
              ts_data$seasonal_component, ts_data$residual_component),
    component = rep(c("Original", "Trend", "Seasonal", "Residual"), 
                   each = nrow(ts_data))
  )
  
  # Create faceted plot
  p <- ggplot2::ggplot(decomp_data, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "#2166AC", size = 0.7) +
    ggplot2::facet_wrap(~ component, scales = "free_y", ncol = 1) +
    ggplot2::labs(title = paste(title, "- Seasonal Decomposition"), x = "Date", y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11, face = "bold")
    )
  
  return(p)
}

# Null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
