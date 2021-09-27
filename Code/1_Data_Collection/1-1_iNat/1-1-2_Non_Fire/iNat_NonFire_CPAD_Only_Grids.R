# CPAD_Only
# Libraries
library(tidyverse)
library(sf)

# Define columns
iNat_Cols <- c("id","iconic_taxon_name","datetime","longitude","latitude","positional_accuracy","quality_grade","Agreement_Score","DateDiff","month")

# Set up
# Make a new variable
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_df %>% 
  # select the data in the defined columns
  dplyr::select(iNat_Cols)%>% 
  # as tibble
  tibble::as_tibble()
# Change any blank values in iconic_taxon_name to No_Name
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 37761530)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 40536233)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 45066625)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 44826420)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 44200439)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 43210211)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 46118622)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 45994055)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 54356125)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 48547486)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 52175222)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 59587902)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 64619675)] <- c("No_Name")
NonFire_CPAD_Only_iNat_shp$iconic_taxon_name[which(NonFire_CPAD_Only_iNat_shp$id == 67276234)] <- c("No_Name")

# Assign a score to quality grade
NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "research")] <- 1
NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "needs_id")] <- 0
NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "casual")] <- 0

# Ensure all coumns are numeric
NonFire_CPAD_Only_iNat_shp <- transform(NonFire_CPAD_Only_iNat_shp, quality_grade = as.numeric(quality_grade))
NonFire_CPAD_Only_iNat_shp <- transform(NonFire_CPAD_Only_iNat_shp, DateDiff = as.numeric(DateDiff))

# make table wider by splitting taxon name data
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_shp %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))

# Add Misisng Columns
NonFire_CPAD_Only_iNat_shp$Plantae <- 0
NonFire_CPAD_Only_iNat_shp$Chromista <- 0

# make a geometry column
NonFire_CPAD_Only_iNat_shp <- st_as_sf(NonFire_CPAD_Only_iNat_shp, coords = c("longitude","latitude"))
# identify crs
st_crs(NonFire_CPAD_Only_iNat_shp)
# change to WGS 84 if needed
NonFire_CPAD_Only_iNat_shp <- st_set_crs(NonFire_CPAD_Only_iNat_shp,"OGC:CRS84")

# Clip to NonFire Extent
# Read in grids shp
Grid_NonFire_CPAD_Only <- 
  sf::st_read("data/Largest_NonFires/Grids/Grid_NonFire_CPAD_Only.shp")
# change ccrs to WGS 84
Grid_NonFire_CPAD_Only <- st_transform(Grid_NonFire_CPAD_Only, 4326)
# clip the data
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_shp[Grid_NonFire_CPAD_Only, ]
# save copy of shapefile
st_write(NonFire_CPAD_Only_iNat_shp, "data/shp/iNat/NonFire/NonFire_CPAD_Only_iNat_shp.shp")

# Filter data to pre-NonFire, NonFire and post-NonFire

# Pre_NonFire
# Filter data to before the month of the NonFire
Pre_NonFire <- NonFire_CPAD_Only_iNat_shp %>% filter(month < 8)
# join data to the grids
iNat_PreNonFire_CPAD_Only_Grids <- sf::st_join(Grid_NonFire_CPAD_Only,Pre_NonFire)
# Remove temp file
rm(Pre_NonFire)

# Find the measures for spatial analysis
# Select key criteria
iNat_PreNonFire_CPAD_Only_Grids <- iNat_PreNonFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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
iNat_PreNonFire_CPAD_Only_Grids[is.na(iNat_PreNonFire_CPAD_Only_Grids)] <- 0

# Replicate the steps for NonFire and post-NonFire data

# NonFire
NonFire <- NonFire_CPAD_Only_iNat_shp %>% filter(month == 8)
iNat_NonFire_CPAD_Only_Grids <- sf::st_join(Grid_NonFire_CPAD_Only,NonFire)
rm(NonFire)

iNat_NonFire_CPAD_Only_Grids <- iNat_NonFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_NonFire_CPAD_Only_Grids[is.na(iNat_NonFire_CPAD_Only_Grids)] <- 0

# Post_NonFire
Post_NonFire <- NonFire_CPAD_Only_iNat_shp %>% filter(month > 8)
iNat_PostNonFire_CPAD_Only_Grids <- sf::st_join(Grid_NonFire_CPAD_Only,Post_NonFire)
rm(Post_NonFire)

iNat_PostNonFire_CPAD_Only_Grids <- iNat_PostNonFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_PostNonFire_CPAD_Only_Grids[is.na(iNat_PostNonFire_CPAD_Only_Grids)] <- 0
