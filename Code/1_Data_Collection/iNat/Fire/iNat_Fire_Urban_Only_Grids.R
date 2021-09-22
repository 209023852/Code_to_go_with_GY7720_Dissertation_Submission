library(tidyverse)
library(sf)

# Urban_Only
iNat_Cols <- c("id","iconic_taxon_name","datetime","longitude","latitude","positional_accuracy","quality_grade","Agreement_Score","DateDiff","month")
# Fire Urban Only
Fire_Urban_Only_iNat_shp <- Fire_Urban_Only_iNat_df %>% 
  dplyr::select(iNat_Cols) %>% 
  tibble::as_tibble()
# Fill Blank with No_Name
Fire_Urban_Only_iNat_shp$iconic_taxon_name[which(Fire_Urban_Only_iNat_shp$id == 46292789)] <- c("No_Name")

Fire_Urban_Only_iNat_shp$quality_grade[which(Fire_Urban_Only_iNat_shp$quality_grade == "research")] <- 1
Fire_Urban_Only_iNat_shp$quality_grade[which(Fire_Urban_Only_iNat_shp$quality_grade == "needs_id")] <- 0
Fire_Urban_Only_iNat_shp$quality_grade[which(Fire_Urban_Only_iNat_shp$quality_grade == "casual")] <- 0

Fire_Urban_Only_iNat_shp <- transform(Fire_Urban_Only_iNat_shp, quality_grade = as.numeric(quality_grade))
Fire_Urban_Only_iNat_shp <- transform(Fire_Urban_Only_iNat_shp, DateDiff = as.numeric(DateDiff))

# Pivot Wider
Fire_Urban_Only_iNat_shp <- Fire_Urban_Only_iNat_shp %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))

# Add Missing Data
Fire_Urban_Only_iNat_shp$Arachnida <- 0
Fire_Urban_Only_iNat_shp$Amphibia <- 0
Fire_Urban_Only_iNat_shp$Actinopterygii <- 0
Fire_Urban_Only_iNat_shp$Protozoa <- 0

# Write a csv as backup
write.csv(Fire_Urban_Only_iNat_shp,"data/shp/iNat/Fire/csv/Fire_Urban_Only_iNat_shp.csv")
Fire_Urban_Only_iNat_shp <- st_as_sf(Fire_Urban_Only_iNat_shp, coords = c("longitude","latitude"))
st_crs(Fire_Urban_Only_iNat_shp)
Fire_Urban_Only_iNat_shp <- st_set_crs(Fire_Urban_Only_iNat_shp,"OGC:CRS84")
# Clip to Fire Extent
Grid_Fire_Urban_Only <- 
  sf::st_read("data/Largest_Fires/New_Grids/Grid_Fire_Urban_Only_250m.shp")
Grid_Fire_Urban_Only <- st_transform(Grid_Fire_Urban_Only, 4326)
Grid_Fire_Urban_Only$id <- seq.int(nrow(Grid_Fire_Urban_Only))
Fire_Urban_Only_iNat_shp <- Fire_Urban_Only_iNat_shp[Grid_Fire_Urban_Only, ]
Fire_Urban_Only_iNat_shp[is.na(Fire_Urban_Only_iNat_shp)] <- 0
st_write(Fire_Urban_Only_iNat_shp, "data/shp/iNat/Fire/Fire_Urban_Only_iNat_shp.shp")
#mapview(Fire_Urban_Only_iNat_shp, map.type="OpenStreetMap")
#mapview(Grid_Fire_Urban_Only, map.type="OpenStreetMap")

# All
iNat_AllFire_Urban_Only_Grids <- sf::st_join(Grid_Fire_Urban_Only,Fire_Urban_Only_iNat_shp)

iNat_AllFire_Urban_Only_Grids <- iNat_AllFire_Urban_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  group_by(id.x) %>%
  summarise(
    avg_positional_accuracy = mean(positional_accuracy, na.rm = F),
    avg_quality_grade = mean(quality_grade, na.rm = F),
    avg_agreement_score = mean(Agreement_Score, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
    aves_count = sum(Aves, na.rm = F),
    plantae_count = sum(Plantae, na.rm = F),
    fungi_count = sum(Fungi, na.rm = F),
    animalia_count = sum(Animalia, na.rm = F),
    insecta_count = sum(Insecta, na.rm = F),
    other_count = sum(No_Name, na.rm = F),
    arachnida_count = sum(Arachnida, na.rm = F),
    reptilia_count = sum(Reptilia, na.rm = F),
    mammalia_count = sum(Mammalia, na.rm = F),
    mollusca_count = sum(Mollusca, na.rm = F),
    chromista_count = sum(Chromista, na.rm = F),
    amphibia_count = sum(Amphibia, na.rm = F),
    actinopterygii_count = sum(Actinopterygii, na.rm = F),
    protozoa_count = sum(Protozoa, na.rm = F), 
    taxon_count = sum(Aves,Plantae,Fungi,Animalia,Insecta,No_Name,Arachnida,Reptilia,Mammalia,Mollusca,Chromista,Amphibia,Actinopterygii,Protozoa, na.rm = F)
  ) %>%
  arrange(id.x)

iNat_AllFire_Urban_Only_Grids[is.na(iNat_AllFire_Urban_Only_Grids)] <- 0

# Pre_Fire
Pre_Fire <- Fire_Urban_Only_iNat_shp %>% filter(month < 6)
iNat_PreFire_Urban_Only_Grids <- sf::st_join(Grid_Fire_Urban_Only,Pre_Fire)
rm(Pre_Fire)

iNat_PreFire_Urban_Only_Grids <- iNat_PreFire_Urban_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  group_by(id.x) %>%
  summarise(
    avg_positional_accuracy = mean(positional_accuracy, na.rm = F),
    avg_quality_grade = mean(quality_grade, na.rm = F),
    avg_agreement_score = mean(Agreement_Score, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
    aves_count = sum(Aves, na.rm = F),
    plantae_count = sum(Plantae, na.rm = F),
    fungi_count = sum(Fungi, na.rm = F),
    animalia_count = sum(Animalia, na.rm = F),
    insecta_count = sum(Insecta, na.rm = F),
    other_count = sum(No_Name, na.rm = F),
    arachnida_count = sum(Arachnida, na.rm = F),
    reptilia_count = sum(Reptilia, na.rm = F),
    mammalia_count = sum(Mammalia, na.rm = F),
    mollusca_count = sum(Mollusca, na.rm = F),
    chromista_count = sum(Chromista, na.rm = F),
    amphibia_count = sum(Amphibia, na.rm = F),
    actinopterygii_count = sum(Actinopterygii, na.rm = F),
    protozoa_count = sum(Protozoa, na.rm = F), 
    taxon_count = sum(Aves,Plantae,Fungi,Animalia,Insecta,No_Name,Arachnida,Reptilia,Mammalia,Mollusca,Chromista,Amphibia,Actinopterygii,Protozoa, na.rm = F)
  ) %>%
  arrange(id.x)

iNat_PreFire_Urban_Only_Grids[is.na(iNat_PreFire_Urban_Only_Grids)] <- 0

# Fire
Fire <- Fire_Urban_Only_iNat_shp %>% filter(month == 6)
iNat_Fire_Urban_Only_Grids <- sf::st_join(Grid_Fire_Urban_Only,Fire)
rm(Fire)

iNat_Fire_Urban_Only_Grids <- iNat_Fire_Urban_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  group_by(id.x) %>%
  summarise(
    avg_positional_accuracy = mean(positional_accuracy, na.rm = F),
    avg_quality_grade = mean(quality_grade, na.rm = F),
    avg_agreement_score = mean(Agreement_Score, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
    aves_count = sum(Aves, na.rm = F),
    plantae_count = sum(Plantae, na.rm = F),
    fungi_count = sum(Fungi, na.rm = F),
    animalia_count = sum(Animalia, na.rm = F),
    insecta_count = sum(Insecta, na.rm = F),
    other_count = sum(No_Name, na.rm = F),
    arachnida_count = sum(Arachnida, na.rm = F),
    reptilia_count = sum(Reptilia, na.rm = F),
    mammalia_count = sum(Mammalia, na.rm = F),
    mollusca_count = sum(Mollusca, na.rm = F),
    chromista_count = sum(Chromista, na.rm = F),
    amphibia_count = sum(Amphibia, na.rm = F),
    actinopterygii_count = sum(Actinopterygii, na.rm = F),
    protozoa_count = sum(Protozoa, na.rm = F), 
    taxon_count = sum(Aves,Plantae,Fungi,Animalia,Insecta,No_Name,Arachnida,Reptilia,Mammalia,Mollusca,Chromista,Amphibia,Actinopterygii,Protozoa, na.rm = F)
  ) %>%
  arrange(id.x)

iNat_Fire_Urban_Only_Grids[is.na(iNat_Fire_Urban_Only_Grids)] <- 0

# Post_Fire
Post_Fire <- Fire_Urban_Only_iNat_shp %>% filter(month > 6)
iNat_PostFire_Urban_Only_Grids <- sf::st_join(Grid_Fire_Urban_Only,Post_Fire)
rm(Post_Fire)

iNat_PostFire_Urban_Only_Grids <- iNat_PostFire_Urban_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  group_by(id.x) %>%
  summarise(
    avg_positional_accuracy = mean(positional_accuracy, na.rm = F),
    avg_quality_grade = mean(quality_grade, na.rm = F),
    avg_agreement_score = mean(Agreement_Score, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
    aves_count = sum(Aves, na.rm = F),
    plantae_count = sum(Plantae, na.rm = F),
    fungi_count = sum(Fungi, na.rm = F),
    animalia_count = sum(Animalia, na.rm = F),
    insecta_count = sum(Insecta, na.rm = F),
    other_count = sum(No_Name, na.rm = F),
    arachnida_count = sum(Arachnida, na.rm = F),
    reptilia_count = sum(Reptilia, na.rm = F),
    mammalia_count = sum(Mammalia, na.rm = F),
    mollusca_count = sum(Mollusca, na.rm = F),
    chromista_count = sum(Chromista, na.rm = F),
    amphibia_count = sum(Amphibia, na.rm = F),
    actinopterygii_count = sum(Actinopterygii, na.rm = F),
    protozoa_count = sum(Protozoa, na.rm = F), 
    taxon_count = sum(Aves,Plantae,Fungi,Animalia,Insecta,No_Name,Arachnida,Reptilia,Mammalia,Mollusca,Chromista,Amphibia,Actinopterygii,Protozoa, na.rm = F)
  ) %>%
  arrange(id.x)

iNat_PostFire_Urban_Only_Grids[is.na(iNat_PostFire_Urban_Only_Grids)] <- 0
