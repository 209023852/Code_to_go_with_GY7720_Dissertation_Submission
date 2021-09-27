#Urban_Only

# Libaries
library(tidyverse) 
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)

# The code below has been adapted by the code sourced at https://rpubs.com/quarcs-lab/spatial-autocorrelation
# To repeat the process for Pre-NonFire and NonFire timeframes the grids and name features will need to be changed. 
# This can be easily done by using the replace tool in RStudios 

#Local Moran I for Version

## 4.1 Moris I Local
# Find Queen Neighbours
PostNonFire_Urban_Only_OSM_neighbours <- poly2nb(OSM_PostNonFire_Urban_Only_Grids, queen = TRUE)
listw <- nb2listw(PostNonFire_Urban_Only_OSM_neighbours)

# Find the  Moran score
globalMoran <- moran.test(OSM_PostNonFire_Urban_Only_Grids$avg_version, listw)
globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

# Develop a map
moran <- moran.plot(OSM_PostNonFire_Urban_Only_Grids$avg_version, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))
local <- localmoran(x = OSM_PostNonFire_Urban_Only_Grids$avg_version, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))
moran.map <- cbind(OSM_PostNonFire_Urban_Only_Grids, local)

# plot the data
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostNonFire_Urban_Only_positional_accuracy")


#Local Moran I for NACount

## 4.1 Moris I Local
PostNonFire_Urban_Only_OSM_neighbours <- poly2nb(OSM_PostNonFire_Urban_Only_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Urban_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostNonFire_Urban_Only_Grids$avg_NACount, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostNonFire_Urban_Only_Grids$avg_NACount, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostNonFire_Urban_Only_Grids$avg_NACount, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostNonFire_Urban_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostNonFire_Urban_Only_quality_grade")

#Local Moran I for DateDiff

## 4.1 Moris I Local
PostNonFire_Urban_Only_OSM_neighbours <- poly2nb(OSM_PostNonFire_Urban_Only_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Urban_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostNonFire_Urban_Only_Grids$avg_datediff, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostNonFire_Urban_Only_Grids$avg_datediff, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostNonFire_Urban_Only_Grids$avg_datediff, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostNonFire_Urban_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostNonFire_Urban_Only_agreement_score")

#Local Moran I for Feature Count

## 4.1 Moris I Local
PostNonFire_Urban_Only_OSM_neighbours <- poly2nb(OSM_PostNonFire_Urban_Only_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Urban_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostNonFire_Urban_Only_Grids$feature_count, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostNonFire_Urban_Only_Grids$feature_count, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostNonFire_Urban_Only_Grids$feature_count, listw = nb2listw(PostNonFire_Urban_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostNonFire_Urban_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostNonFire_Urban_Only_feature_count")

# Getis-Orb. One was completed as all inputs have the same output

## 4.2 Getis-ord
# Find centroids
coords <- st_centroid(st_geometry(OSM_PostNonFire_Urban_Only_Grids))
# Calculate nearneighbours
nb <- dnearneigh(coords, 0, 800)
nb_lw <- nb2listw(nb, style = 'B')

# Find LocalG score
local_g <- localG(OSM_PostNonFire_Urban_Only_Grids$feature_count, nb_lw)
local_g <- cbind(OSM_PostNonFire_Urban_Only_Grids, as.matrix(local_g))
names(local_g)[6] <- "gstat"

# Plot the Map
tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty",
          title = "OSM_PostNonFire_Urban_Only_Getis_ord") +
  tm_borders(alpha=.4)