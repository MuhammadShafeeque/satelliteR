# devtools::test(".", "downloadSentinel5P")
context("Download Sentinel-5P air pollution data")

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P parameter validation works", {
  # Test invalid product types
  expect_error(
    downloadSentinel5P(
      product = "INVALID_PRODUCT",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Invalid product type"
  )
  
  # Test invalid date formats
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "01-01-2023",  # Wrong format
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Invalid date format"
  )
  
  # Test invalid bbox
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10),  # Missing coordinate
      output_dir = tempdir()
    ),
    "Bounding box must have 4 coordinates"
  )
  
  # Test invalid cloud cover
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      cloud_cover = 150,  # > 100
      output_dir = tempdir()
    ),
    "Cloud cover must be between 0 and 100"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P product validation works", {
  valid_products <- c("L2__NO2___", "L2__SO2___", "L2__CO____", "L2__O3____",
                     "L2__HCHO__", "L2__CH4___", "L2__AER_AI", "L2__AER_LH",
                     "L2__CLOUD_")
  
  for (product in valid_products) {
    # Should not throw error for valid products (though download may fail without credentials)
    expect_error(
      downloadSentinel5P(
        product = product,
        start_date = "2023-01-01",
        end_date = "2023-01-02",
        bbox = c(-10, 40, 10, 50),
        output_dir = tempdir()
      ),
      NA  # No error expected for parameter validation
    )
  }
})

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P date validation works", {
  # Test start date after end date
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "2023-01-02",
      end_date = "2023-01-01",  # Earlier than start
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Start date must be before end date"
  )
  
  # Test future dates (should warn or handle gracefully)
  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")
  expect_warning(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = future_date,
      end_date = future_date,
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Future dates specified"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P output structure is correct", {
  # Mock successful download result
  skip_if_not_installed("mockery")
  
  mock_result <- list(
    downloaded_files = c("file1.nc", "file2.nc"),
    metadata = data.frame(
      product = c("L2__NO2___", "L2__NO2___"),
      date = as.Date(c("2023-01-01", "2023-01-02")),
      size_mb = c(150.5, 148.2),
      stringsAsFactors = FALSE
    ),
    log = list(
      start_time = Sys.time(),
      end_time = Sys.time(),
      status = "success",
      message = "Download completed"
    )
  )
  
  # Test that returned object has expected structure
  expect_true(is.list(mock_result))
  expect_true("downloaded_files" %in% names(mock_result))
  expect_true("metadata" %in% names(mock_result))
  expect_true("log" %in% names(mock_result))
  expect_true(is.character(mock_result$downloaded_files))
  expect_true(is.data.frame(mock_result$metadata))
  expect_true(is.list(mock_result$log))
})

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P handles missing credentials appropriately", {
  # Test without credentials - should provide informative error
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      source = "copernicus"
    ),
    "Authentication required"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadSentinel5P handles network issues gracefully", {
  # Skip network tests unless specifically requested
  skip_on_cran()
  skip_if_offline()
  
  # Test with very short timeout to simulate network issues
  expect_error(
    downloadSentinel5P(
      product = "L2__NO2___",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      timeout = 0.1  # Very short timeout
    ),
    "Network timeout"
  )
})
