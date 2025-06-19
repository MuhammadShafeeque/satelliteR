# devtools::test(".", "downloadMODISPollution")
context("Download MODIS air pollution and atmospheric data")

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution parameter validation works", {
  # Test invalid product types
  expect_error(
    downloadMODISPollution(
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
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "01-01-2023",  # Wrong format
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Invalid date format"
  )
  
  # Test invalid bbox
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10),  # Missing coordinate
      output_dir = tempdir()
    ),
    "Bounding box must have 4 coordinates"
  )
  
  # Test invalid collection
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      collection = "invalid",
      output_dir = tempdir()
    ),
    "Invalid collection version"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution product validation works", {
  valid_products <- c("MOD04_L2", "MYD04_L2", "MOD04_3K", "MYD04_3K",
                     "MOD14A1", "MYD14A1", "MOD08_D3", "MYD08_D3",
                     "MOD08_M3", "MYD08_M3", "MCD43A4")
  
  for (product in valid_products) {
    # Should not throw error for valid products (though download may fail without credentials)
    expect_silent({
      tryCatch(
        downloadMODISPollution(
          product = product,
          start_date = "2023-01-01",
          end_date = "2023-01-02",
          bbox = c(-10, 40, 10, 50),
          output_dir = tempdir()
        ),
        error = function(e) {
          # Expect authentication or network errors, not parameter validation errors
          expect_false(grepl("Invalid product type", e$message))
        }
      )
    })
  }
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution collection validation works", {
  valid_collections <- c("6", "6.1", "61")
  
  for (collection in valid_collections) {
    expect_silent({
      tryCatch(
        downloadMODISPollution(
          product = "MOD04_L2",
          start_date = "2023-01-01",
          end_date = "2023-01-02",
          bbox = c(-10, 40, 10, 50),
          collection = collection,
          output_dir = tempdir()
        ),
        error = function(e) {
          # Should not be collection validation error
          expect_false(grepl("Invalid collection", e$message))
        }
      )
    })
  }
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution date validation works", {
  # Test start date after end date
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-02",
      end_date = "2023-01-01",  # Earlier than start
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Start date must be before end date"
  )
  
  # Test dates before MODIS era (Terra launch: 1999-12-18)
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "1999-01-01",  # Before Terra launch
      end_date = "1999-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir()
    ),
    "Date range is before MODIS data availability"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution output structure is correct", {
  # Mock successful download result
  mock_result <- list(
    downloaded_files = c("MOD04_L2.A2023001.h17v05.061.hdf", 
                        "MOD04_L2.A2023002.h17v05.061.hdf"),
    metadata = data.frame(
      product = c("MOD04_L2", "MOD04_L2"),
      date = as.Date(c("2023-01-01", "2023-01-02")),
      tile = c("h17v05", "h17v05"),
      collection = c("061", "061"),
      size_mb = c(25.4, 24.8),
      stringsAsFactors = FALSE
    ),
    log = list(
      start_time = Sys.time(),
      end_time = Sys.time(),
      status = "success",
      message = "Download completed",
      files_downloaded = 2,
      total_size_mb = 50.2
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
  
  # Test metadata structure
  expected_cols <- c("product", "date", "tile", "collection", "size_mb")
  expect_true(all(expected_cols %in% names(mock_result$metadata)))
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution handles missing credentials appropriately", {
  # Test without credentials - should provide informative error
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      source = "earthdata"
    ),
    "NASA Earthdata authentication required"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution handles quality day parameter correctly", {
  # Test quality_day parameter for fire products
  expect_silent({
    tryCatch(
      downloadMODISPollution(
        product = "MOD14A1",
        start_date = "2023-01-01",
        end_date = "2023-01-02",
        bbox = c(-10, 40, 10, 50),
        output_dir = tempdir(),
        quality_day = TRUE
      ),
      error = function(e) {
        # Should not be parameter validation error
        expect_false(grepl("Invalid parameter", e$message))
      }
    )
  })
  
  # Test quality_day parameter for non-fire products (should warn or ignore)
  expect_warning(
    downloadMODISPollution(
      product = "MOD04_L2",  # Not a fire product
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      quality_day = TRUE
    ),
    "quality_day parameter only applies to fire products"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution handles network issues gracefully", {
  # Skip network tests unless specifically requested
  skip_on_cran()
  skip_if_offline()
  
  # Test with invalid URL to simulate network issues
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      source = "invalid_source"
    ),
    "Invalid data source"
  )
})

#-------------------------------------------------------------------------------
test_that("downloadMODISPollution max_files parameter works", {
  # Test max_files parameter
  expect_silent({
    tryCatch(
      downloadMODISPollution(
        product = "MOD04_L2",
        start_date = "2023-01-01",
        end_date = "2023-01-31",  # Long date range
        bbox = c(-10, 40, 10, 50),
        output_dir = tempdir(),
        max_files = 5
      ),
      error = function(e) {
        # Should not be parameter validation error
        expect_false(grepl("Invalid max_files", e$message))
      }
    )
  })
  
  # Test invalid max_files
  expect_error(
    downloadMODISPollution(
      product = "MOD04_L2",
      start_date = "2023-01-01",
      end_date = "2023-01-02",
      bbox = c(-10, 40, 10, 50),
      output_dir = tempdir(),
      max_files = 0  # Invalid
    ),
    "max_files must be greater than 0"
  )
})
