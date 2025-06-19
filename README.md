#### Package downloads from the [RStudio CRAN Mirror](http://cran-logs.rstudio.com/)</b>

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/satellite) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/satellite)

<hr>

# satellite
This smorgasbord provides a variety of functions which are useful 
for handling, manipulating and visualizing remote sensing data, including
comprehensive air pollution monitoring capabilities.

## Key Features

### Traditional Remote Sensing
- **Landsat support**: Complete workflow for Landsat 4-8 data processing
- **Atmospheric correction**: DOS2/DOS4 models for atmospheric scattering correction
- **Topographic correction**: Terrain-based illumination corrections
- **Calibration**: DN to radiance/reflectance/brightness temperature conversions

### Air Pollution Monitoring (NEW)
- **Sentinel-5P TROPOMI**: Download and process air pollution data (NO2, SO2, CO, O3, HCHO, CH4)
- **MODIS Atmospheric**: Access Terra/Aqua MODIS aerosol and fire data
- **Pollution Statistics**: Comprehensive statistical analysis of air quality data
- **Time Series Analysis**: Trend detection and anomaly identification
- **Quality Assurance**: Advanced filtering and validation capabilities

### Supported Pollutants
- **Nitrogen Dioxide (NO2)**: Tropospheric and stratospheric columns
- **Sulfur Dioxide (SO2)**: Total vertical columns
- **Carbon Monoxide (CO)**: Total column measurements  
- **Ozone (O3)**: Total vertical columns
- **Formaldehyde (HCHO)**: Tropospheric columns
- **Methane (CH4)**: Mixing ratios
- **Aerosol Optical Depth (AOD)**: MODIS aerosol products
- **PM2.5**: Estimated from aerosol data

====

### Installation

For the stable release version of **satellite** use


```S
install.packages("satellite")
```


To install the development version use


```S
library(devtools)
install_github("environmentalinformatics-marburg/satellite", ref = "develop")
```


### Contact

Please file bug reports and feature requests at https://github.com/environmentalinformatics-marburg/satellite/issues