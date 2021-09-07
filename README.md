# inforSAT

InforSAT wants to leverage open source tools for optimally management and analysis of raster imagery data.

In particular it uses the well-known GDAL and Mapserver suites to do band maths and extract and view indices over a Leaflet webgis framework. R provides the processing and parallellization backend, and Shiny-R the infrastructure for connecting web widgets to processes. 

InforSAT wants to provide users with means to collect spatio-temporal data from satellite imagery quickly, but keeping track of reliability and accuracy loss due to clouds, shade and other atmospheric or other disturbance. 

Sentinel-2 Level 2A image products with cloud coverage below 10% are automatically downloaded from ESA's SciHUB REST API, and indexed by InforSAT for the four tiles that cover Veneto Region. InforSAT automatically picks up images in a specific folder that is defined in global.R, a script reads all images and collects data that is used later for scaling and processing.

Users can choose what index they want to view, or put their own formula and/or analyze over all available image dates for user-defined specific polygons.


Data are processed on the original images in JP2 format, at 10/20 m resolution depending on the index.







