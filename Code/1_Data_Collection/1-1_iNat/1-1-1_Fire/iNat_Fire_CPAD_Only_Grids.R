# CPAD_Only
# Libraries
library(tidyverse)
library(sf)

# Define columns
iNat_Cols <- c("id","iconic_taxon_name","datetime","longitude","latitude","positional_accuracy","quality_grade","Agreement_Score","DateDiff","month")

# Set up
# Make a new variable
Fire_CPAD_Only_iNat_shp <- Fire_CPAD_Only_iNat_df %>% 
  # select the data in the defined columns
  dplyr::select(iNat_Cols)%>% 
  # as tibble
  tibble::as_tibble()
# Change any blank values in iconic_taxon_name to No_Name
Fire_CPAD_Only_iNat_shp$iconic_taxon_name[which(Fire_CPAD_Only_iNat_shp$id == 52481274)] <- c("No_Name")

# Assign a score to quality grade
Fire_CPAD_Only_iNat_shp$quality_grade[which(Fire_CPAD_Only_iNat_shp$quality_grade == "research")] <- 1
Fire_CPAD_Only_iNat_shp$quality_grade[which(Fire_CPAD_Only_iNat_shp$quality_grade == "needs_id")] <- 0
Fire_CPAD_Only_iNat_shp$quality_grade[which(Fire_CPAD_Only_iNat_shp$quality_grade == "casual")] <- 0

# Ensure all coumns are numeric
Fire_CPAD_Only_iNat_shp <- transform(Fire_CPAD_Only_iNat_shp, quality_grade = as.numeric(quality_grade))
Fire_CPAD_Only_iNat_shp <- transform(Fire_CPAD_Only_iNat_shp, DateDiff = as.numeric(DateDiff))

# make table wider by splitting taxon name data
Fire_CPAD_Only_iNat_shp <- Fire_CPAD_Only_iNat_shp %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))

# Add Misisng Columns
Fire_CPAD_Only_iNat_shp$Chromista <- 0

# make a geometry column
Fire_CPAD_Only_iNat_shp <- st_as_sf(Fire_CPAD_Only_iNat_shp, coords = c("longitude","latitude"))
# identify crs
st_crs(Fire_CPAD_Only_iNat_shp)
# change to WGS 84 if needed
Fire_CPAD_Only_iNat_shp <- st_set_crs(Fire_CPAD_Only_iNat_shp,"OGC:CRS84")

# Clip to Fire Extent
# Read in grids shp
Grid_Fire_CPAD_Only <- 
  sf::st_read("data/Largest_Fires/Grids/Grid_Fire_CPAD_Only.shp")
# change ccrs to WGS 84
Grid_Fire_CPAD_Only <- st_transform(Grid_Fire_CPAD_Only, 4326)
# clip the data
Fire_CPAD_Only_iNat_shp <- Fire_CPAD_Only_iNat_shp[Grid_Fire_CPAD_Only, ]
# save copy of shapefile
st_write(Fire_CPAD_Only_iNat_shp, "data/shp/iNat/Fire/Fire_CPAD_Only_iNat_shp.shp")

# Filter data to pre-fire, fire and post-fire

# Pre_Fire
# Filter data to before the month of the fire
Pre_Fire <- Fire_CPAD_Only_iNat_shp %>% filter(month < 8)
# join data to the grids
iNat_PreFire_CPAD_Only_Grids <- sf::st_join(Grid_Fire_CPAD_Only,Pre_Fire)
# Remove temp file
rm(Pre_Fire)

# Find the measures for spatial analysis
# Select key criteria
iNat_PreFire_CPAD_Only_Grids <- iNat_PreFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  # group by id
  group_by(id.x) %>%
  # find averages of all quality checks and sum of all counts
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
  # Arrange by id
  arrange(id.x)

# Make all NA values 0
iNat_PreFire_CPAD_Only_Grids[is.na(iNat_PreFire_CPAD_Only_Grids)] <- 0

# Replicate the steps for fire and post-fire data

# Fire
Fire <- Fire_CPAD_Only_iNat_shp %>% filter(month == 8)
iNat_Fire_CPAD_Only_Grids <- sf::st_join(Grid_Fire_CPAD_Only,Fire)
rm(Fire)

iNat_Fire_CPAD_Only_Grids <- iNat_Fire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_Fire_CPAD_Only_Grids[is.na(iNat_Fire_CPAD_Only_Grids)] <- 0

# Post_Fire
Post_Fire <- Fire_CPAD_Only_iNat_shp %>% filter(month > 8)
iNat_PostFire_CPAD_Only_Grids <- sf::st_join(Grid_Fire_CPAD_Only,Post_Fire)
rm(Post_Fire)

iNat_PostFire_CPAD_Only_Grids <- iNat_PostFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_PostFire_CPAD_Only_Grids[is.na(iNat_PostFire_CPAD_Only_Grids)] <- 0
