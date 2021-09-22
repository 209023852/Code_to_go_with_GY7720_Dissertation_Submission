#PreNonFire Neither

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

# iNat_PostNFN_APA = iNat_PreNonFire_Neither_avg_positional_accuracy
## 4.1 Moris I Local
PreNonFire_Neither_iNat_neighbours <- poly2nb(iNat_PreNonFire_Neither_Grids, queen = TRUE)

listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE)

globalMoran <- moran.test(iNat_PreNonFire_Neither_Grids$avg_positional_accuracy, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PreNonFire_Neither_Grids$avg_positional_accuracy, listw = nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

local <- localmoran(x = iNat_PreNonFire_Neither_Grids$avg_positional_accuracy, listw = nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

moran.map <- cbind(iNat_PreNonFire_Neither_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PreNonFire_Neither_positional_accuracy")


# iNat_PostNFN_AQG = iNat_PreNonFire_Neither_avg_quality_grade
## 4.1 Moris I Local
PreNonFire_Neither_iNat_neighbours <- poly2nb(iNat_PreNonFire_Neither_Grids, queen = TRUE)

listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE)

globalMoran <- moran.test(iNat_PreNonFire_Neither_Grids$avg_quality_grade, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PreNonFire_Neither_Grids$avg_quality_grade, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

local <- localmoran(x = iNat_PreNonFire_Neither_Grids$avg_quality_grade, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

moran.map <- cbind(iNat_PreNonFire_Neither_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PreNonFire_Neither_quality_grade")

# iNat_PostNFN_AAS = iNat_PreNonFire_Neither_avg_agreement_score
## 4.1 Moris I Local
PreNonFire_Neither_iNat_neighbours <- poly2nb(iNat_PreNonFire_Neither_Grids, queen = TRUE)

listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE)

globalMoran <- moran.test(iNat_PreNonFire_Neither_Grids$avg_agreement_score, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PreNonFire_Neither_Grids$avg_agreement_score, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

local <- localmoran(x = iNat_PreNonFire_Neither_Grids$avg_agreement_score, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

moran.map <- cbind(iNat_PreNonFire_Neither_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PreNonFire_Neither_agreement_score")

# iNat_PostNFN_ADD = iNat_PreNonFire_Neither_avg_datediff
## 4.1 Moris I Local
PreNonFire_Neither_iNat_neighbours <- poly2nb(iNat_PreNonFire_Neither_Grids, queen = TRUE)

listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE)

globalMoran <- moran.test(iNat_PreNonFire_Neither_Grids$avg_datediff, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PreNonFire_Neither_Grids$avg_datediff, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

local <- localmoran(x = iNat_PreNonFire_Neither_Grids$avg_datediff, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

moran.map <- cbind(iNat_PreNonFire_Neither_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PreNonFire_Neither_datediff")

# iNat_PostNFN_TC = iNat_PreNonFire_Neither_taxon_count
## 4.1 Moris I Local
PreNonFire_Neither_iNat_neighbours <- poly2nb(iNat_PreNonFire_Neither_Grids, queen = TRUE)

listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE)

globalMoran <- moran.test(iNat_PreNonFire_Neither_Grids$taxon_count, listw)

globalMoran
globalMoran[["estimate"]][["Moran I statistic"]]
globalMoran[["p.value"]]

moran <- moran.plot(iNat_PreNonFire_Neither_Grids$taxon_count, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

local <- localmoran(x = iNat_PreNonFire_Neither_Grids$taxon_count, listw = listw <- nb2listw(PreNonFire_Neither_iNat_neighbours, style = "W", zero.policy = TRUE))

moran.map <- cbind(iNat_PreNonFire_Neither_Grids, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          midpoint = NA,
          title = "iNat_PreNonFire_Neither_taxon_count")

## 4.2 Getis-ord
coords <- st_centroid(st_geometry(iNat_PreNonFire_Neither_Grids))
nb <- dnearneigh(coords, 0, 800)
nb_lw <- nb2listw(nb, style = 'B')

local_g <- localG(iNat_PreNonFire_Neither_Grids$taxon_count, nb_lw)
local_g <- cbind(iNat_PreNonFire_Neither_Grids, as.matrix(local_g))
names(local_g)[6] <- "gstat"

tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty",
          title = "iNat_PreNonFire_Neither_Getis_ord") +
  tm_borders(alpha=.4)
