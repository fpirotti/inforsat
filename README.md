# InforSAT

## Key concepts

InforSAT uses state of the art open source engines to 
 - collect and organize satellite imagery data 
 - provide tools for learning basic concepts related to image analysis, such as
    - creating false color composites
    - band math for creating indices
    - extract zonal statistics from user-defined areas over the full available timeline
    - download timeseries of index values for further processing

InforSAT wants to provide users with means to collect spatio-temporal data from satellite imagery quickly, but keeping track of reliability and accuracy loss due to clouds, shade and other atmospheric or other disturbance. 

## Behind the scenes

InforSAT uses GDAL and Mapserver libraries to do band maths and extract and view indices over a Leaflet webgis framework. R provides the processing and parallellization backend, and Shiny-R the infrastructure for connecting web widgets to processes. 


Sentinel-2 Level 2A image products with cloud coverage below 10% are chosen for populating the InforSAT infrastructure. Image management is file-based, as images are downloaded from ESA's SciHUB REST API, and indexed by InforSAT for the four tiles that cover Veneto Region. InforSAT automatically picks up images in a specific folder that is defined in the configuration file (config.conf), and a script reads all images and collects metadata that is used later for scaling and processing.

Data are processed on the original images in JP2 format, at 10/20 m resolution depending on the index.







