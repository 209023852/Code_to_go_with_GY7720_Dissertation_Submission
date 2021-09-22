#PostFire CPAD_Only

#The code used in this section ahs been adapted by the code sourced at https://rpubs.com/quarcs-lab/spatial-autocorrelation

library(tidyverse)  # Modern data science workflow
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

# OSM_PreFN_APA = OSM_PostFire_CPAD_Only_avg_version
## 4.1 Moris I Local
PostFire_CPAD_Only_OSM_neighbours <- poly2nb(OSM_PostFire_CPAD_Only_Grids, queen = TRUE)

listw <- nb2listw(PostFire_CPAD_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostFire_CPAD_Only_Grids$avg_version, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostFire_CPAD_Only_Grids$avg_version, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostFire_CPAD_Only_Grids$avg_version, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostFire_CPAD_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostFire_CPAD_Only_version")

# OSM_PreFN_AQG = OSM_PostFire_CPAD_Only_avg_NACount
## 4.1 Moris I Local
PostFire_CPAD_Only_OSM_neighbours <- poly2nb(OSM_PostFire_CPAD_Only_Grids, queen = TRUE)

listw <- nb2listw(PostFire_CPAD_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostFire_CPAD_Only_Grids$avg_NACount, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostFire_CPAD_Only_Grids$avg_NACount, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostFire_CPAD_Only_Grids$avg_NACount, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostFire_CPAD_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostFire_CPAD_Only_NACount")

# OSM_PreFN_ADD = OSM_PostFire_CPAD_Only_avg_datediff
## 4.1 Moris I Local
PostFire_CPAD_Only_OSM_neighbours <- poly2nb(OSM_PostFire_CPAD_Only_Grids, queen = TRUE)

listw <- nb2listw(PostFire_CPAD_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostFire_CPAD_Only_Grids$avg_datediff, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostFire_CPAD_Only_Grids$avg_datediff, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostFire_CPAD_Only_Grids$avg_datediff, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostFire_CPAD_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostFire_CPAD_Only_datediff")



# OSM_PreFN_TC = OSM_PostFire_CPAD_Only_feature_count
## 4.1 Moris I Local
PostFire_CPAD_Only_OSM_neighbours <- poly2nb(OSM_PostFire_CPAD_Only_Grids, queen = TRUE)

listw <- nb2listw(PostFire_CPAD_Only_OSM_neighbours)

globalMoran <- moran.test(OSM_PostFire_CPAD_Only_Grids$feature_count, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(OSM_PostFire_CPAD_Only_Grids$feature_count, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

local <- localmoran(x = OSM_PostFire_CPAD_Only_Grids$feature_count, listw = nb2listw(PostFire_CPAD_Only_OSM_neighbours, style = "W"))

moran.map <- cbind(OSM_PostFire_CPAD_Only_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "OSM_PostFire_CPAD_Only_feature_count")

## 4.2 Getis-ord
coords <- st_centroid(st_geometry(OSM_PostFire_CPAD_Only_Grids))
nb <- dnearneigh(coords, 0, 800)
nb_lw <- nb2listw(nb, style = 'B')

local_g <- localG(OSM_PostFire_CPAD_Only_Grids$feature_count, nb_lw)
local_g <- cbind(OSM_PostFire_CPAD_Only_Grids, as.matrix(local_g))
names(local_g)[6] <- "gstat"

tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty",
          title = "OSM_PostFire_CPAD_Only_Getis_ord") +
  tm_borders(alpha=.4)
