# Data README
This README file has been added to replace the data used in this study. The removal of the original data is for data protection reasons alone and all sources on where the data was obtained have been linked below. 

# Non-API Data

## Califonia_County_Boundaries
The California Open Data Portal contains boundary data of California at a County, State and Place scale. This study used the County scale as the respective county would be used in the creation of the non-Fire grids. 
This data is within a shapefile format and can be accessed at https://data.ca.gov/dataset/ca-geographic-boundaries
The open-source license for the Califonia_County_Boundaries is under the Open Government Data Act as it is a part of the data sourced from the California Open Data Portal.

## Adjusted_Urban_Areas
The Adjusted_Urban_Areas shapefile was sourced from the California State Geoportal. The shapefile is the adjusted urban areas of California that was established by the FHWA in 2010. The data is 11 years out of date, it has been regularly updated. 
The data can be sourced on the California State Geoportal at https://gis.data.ca.gov/datasets/51e54198fb68443cb0d73390ec46f364_0/explore?location=37.082412%2C-119.393250%2C6.85
This data is under the Creative Commons Attribution used by the California State Geoportal. 

## California_Protected_Area_Database
The California Protected Area Database is a shapefile of the document protected regions of California. 
The shapefile has been sourced from the California State Geoportal at https://gis.data.ca.gov/datasets/6b4a47259383432990de2b858e07c988_2/explore
This data is under the Creative Commons Attribution used by the California State Geoportal. 

## Wildfire_Perimeters
The Wildfire Perimeter data is all documented wildland fires across the whole United States. This file is regularly updated. The data was sourced from the California Department of Forestry and Fire Protection and when downloaded was filtered to only include 2020 dates. 
The shapefile has been sourced from the California State Geoportal at https://gis.data.ca.gov/datasets/f72ebe741e3b4f0db376b4e765728339_0/explore?location=45.092650%2C-113.731550%2C4.57
This data is under the Creative Commons Attribution used by the California State Geoportal. 

## Wildfire_Incidents
The Wildland Fire Locations Full History data contains all information on fire events across the United States. This data was filtered to only include California and the year 2020. 
The shapefile has been sourced from the California State Geoportal at https://data-nifc.opendata.arcgis.com/datasets/d8fdb82b3931403db3ef47744c825fbf_0/explore
This data is under the Open Government Data Act used by the National Interagency Fire Center. 

# API Data

## rinat library
The iNaturalist data was sourced from the R library rinat. The rinat library has a built in API that allows for direct access to the whole iNaturalist catalogue. 
The pdf for the rinat library can be found at https://cran.r-project.org/web/packages/rinat/rinat.pdf
iNaturalist is found at the url www.inaturalist.org/
iNaturalist information is under the Creative Commons Attribution-NonCommercial 4.0 International license

## OHSOME_API
OpenStreetMap has many built in R libraries to access data, but this study required the Full History data resulting in the OpenStreetMap History Data Analytics Platform (OHSOME) API being chosen. 
This API was created by the Heidelberg Institute for Geoinformation Technology and documentation on the API can be found at https://heigit.org/big-spatial-data-analytics-en/ohsome/
OpenStreetMap itself can be found at https://www.openstreetmap.org
All OpenStreetMap data is open source under the Creative Commons Attribution-ShareAlike 2.0 licence
