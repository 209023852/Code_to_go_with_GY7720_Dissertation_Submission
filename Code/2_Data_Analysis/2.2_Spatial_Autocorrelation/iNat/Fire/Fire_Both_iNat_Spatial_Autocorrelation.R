#PostFire Both

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

# iNat_PreFN_APA = iNat_PostFire_Both_avg_positional_accuracy
## 4.1 Moris I Local
PostFire_Both_iNat_neighbours <- poly2nb(iNat_PostFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostFire_Both_Grids$avg_positional_accuracy, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostFire_Both_Grids$avg_positional_accuracy, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostFire_Both_Grids$avg_positional_accuracy, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostFire_Both_positional_accuracy")


## 4.1 Moris I Local
PostFire_Both_iNat_neighbours <- poly2nb(iNat_PostFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostFire_Both_Grids$avg_quality_grade, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostFire_Both_Grids$avg_quality_grade, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostFire_Both_Grids$avg_quality_grade, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostFire_Both_quality_grade")

## 4.1 Moris I Local
PostFire_Both_iNat_neighbours <- poly2nb(iNat_PostFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostFire_Both_Grids$avg_agreement_score, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostFire_Both_Grids$avg_agreement_score, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostFire_Both_Grids$avg_agreement_score, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostFire_Both_agreement_score")

## 4.1 Moris I Local
PostFire_Both_iNat_neighbours <- poly2nb(iNat_PostFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostFire_Both_Grids$avg_datediff, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostFire_Both_Grids$avg_datediff, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostFire_Both_Grids$avg_datediff, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostFire_Both_datediff")


## 4.1 Moris I Local
PostFire_Both_iNat_neighbours <- poly2nb(iNat_PostFire_Both_Grids, queen = TRUE)

listw <- nb2listw(PostFire_Both_iNat_neighbours)

globalMoran <- moran.test(iNat_PostFire_Both_Grids$taxon_count, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PostFire_Both_Grids$taxon_count, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

local <- localmoran(x = iNat_PostFire_Both_Grids$taxon_count, listw = nb2listw(PostFire_Both_iNat_neighbours, style = "W"))

moran.map <- cbind(iNat_PostFire_Both_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PostFire_Both_taxon_count")

## 4.2 Getis-ord
coords <- st_centroid(st_geometry(iNat_PostFire_Both_Grids))
nb <- dnearneigh(coords, 0, 800)
nb_lw <- nb2listw(nb, style = 'B')

local_g <- localG(iNat_PostFire_Both_Grids$taxon_count, nb_lw)
local_g <- cbind(iNat_PostFire_Both_Grids, as.matrix(local_g))
names(local_g)[6] <- "gstat"

tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty",
          title = "iNat_PostFire_Both_Getis_ord") +
  tm_borders(alpha=.4)


