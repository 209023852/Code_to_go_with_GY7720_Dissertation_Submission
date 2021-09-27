#Both

# Libraries
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

#Local Moran I for Positional Accuracy

## 4.1 Moris I Local
# Find Queen Neighbours
PostNonFire_Both_iNat_neighbours <- poly2nb(iNat_PostNonFire_Both_Grids, queen = TRUE)
listw <- nb2listw(PostNonFire_Both_iNat_neighbours)

# Find the  Moran score
globalMoran <- moran.test(iNat_PostNonFire_Both_Grids$avg_positional_accuracy, listw)
globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

# Develop a map
moran <- moran.plot(iNat_PostNonFire_Both_Grids$avg_positional_accuracy, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))
local <- localmoran(x = iNat_PostNonFire_Both_Grids$avg_positional_accuracy, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))
moran.map <- cbind(iNat_PostNonFire_Both_Grids, local)

# plot the data
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostNonFire_Both_positional_accuracy")


#Local Moran I for Quality Grade

## 4.1 Moris I Local
PostNonFire_Both_iNat_neighbours <- poly2nb(iNat_PostNonFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostNonFire_Both_Grids$avg_quality_grade, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostNonFire_Both_Grids$avg_quality_grade, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostNonFire_Both_Grids$avg_quality_grade, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostNonFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostNonFire_Both_quality_grade")

#Local Moran I for Agreement Score

## 4.1 Moris I Local
PostNonFire_Both_iNat_neighbours <- poly2nb(iNat_PostNonFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostNonFire_Both_Grids$avg_agreement_score, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostNonFire_Both_Grids$avg_agreement_score, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostNonFire_Both_Grids$avg_agreement_score, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostNonFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostNonFire_Both_agreement_score")

#Local Moran I for DateDiff

## 4.1 Moris I Local
PostNonFire_Both_iNat_neighbours <- poly2nb(iNat_PostNonFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostNonFire_Both_Grids$avg_datediff, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostNonFire_Both_Grids$avg_datediff, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostNonFire_Both_Grids$avg_datediff, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostNonFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostNonFire_Both_datediff")


#Local Moran I for Taxon Count

## 4.1 Moris I Local
PostNonFire_Both_iNat_neighbours <- poly2nb(iNat_PostNonFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostNonFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostNonFire_Both_Grids$taxon_count, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostNonFire_Both_Grids$taxon_count, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostNonFire_Both_Grids$taxon_count, listw = nb2listw(PostNonFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostNonFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostNonFire_Both_taxon_count")

# Getis-Orb. One was completed as all inputs have the same output

## 4.2 Getis-ord
# Find centroids
coords <- st_centroid(st_geometry(iNat_PostNonFire_Both_Grids))
# Calculate nearneighbours
nb <- dnearneigh(coords, 0, 800)
nb_lw <- nb2listw(nb, style = 'B')

# Find LocalG score
local_g <- localG(iNat_PostNonFire_Both_Grids$taxon_count, nb_lw)
local_g <- cbind(iNat_PostNonFire_Both_Grids, as.matrix(local_g))
names(local_g)[6] <- "gstat"

# Plot the Map
tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty",
          title = "iNat_PostNonFire_Both_Getis_ord") +
  tm_borders(alpha=.4)