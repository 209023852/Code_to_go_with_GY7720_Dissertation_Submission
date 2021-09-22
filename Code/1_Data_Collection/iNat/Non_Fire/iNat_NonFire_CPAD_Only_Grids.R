library(tidyverse)
library(sf)

# CPAD_Only
iNat_Cols <- c("id","iconic_taxon_name","datetime","longitude","latitude","positional_accuracy","quality_grade","Agreement_Score","DateDiff","month")
# NonFire Urban Only
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_df %>% 
  dplyr::select(iNat_Cols) %>% 
  tibble::as_tibble()
# Fill Blank with No_Name
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


NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "research")] <- 1
NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "needs_id")] <- 0
NonFire_CPAD_Only_iNat_shp$quality_grade[which(NonFire_CPAD_Only_iNat_shp$quality_grade == "casual")] <- 0

NonFire_CPAD_Only_iNat_shp <- transform(NonFire_CPAD_Only_iNat_shp, quality_grade = as.numeric(quality_grade))
NonFire_CPAD_Only_iNat_shp <- transform(NonFire_CPAD_Only_iNat_shp, DateDiff = as.numeric(DateDiff))

# Pivot Wider
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_shp %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))

# Add Missing Data
NonFire_CPAD_Only_iNat_shp$Plantae <- 0
NonFire_CPAD_Only_iNat_shp$Chromista <- 0

# Write a csv as backup
write.csv(NonFire_CPAD_Only_iNat_shp,"data/shp/iNat/NonFire/csv/NonFire_CPAD_Only_iNat_shp.csv")
NonFire_CPAD_Only_iNat_shp <- st_as_sf(NonFire_CPAD_Only_iNat_shp, coords = c("longitude","latitude"))
st_crs(NonFire_CPAD_Only_iNat_shp)
NonFire_CPAD_Only_iNat_shp <- st_set_crs(NonFire_CPAD_Only_iNat_shp,"OGC:CRS84")
# Clip to NonFire Extent
Grid_NonFire_CPAD_Only <- 
  sf::st_read("data/Largest_Fires/Grids/Test_Grid_Fire_CPAD_Only_County.shp")
Grid_NonFire_CPAD_Only <- st_transform(Grid_NonFire_CPAD_Only, 4326)
Grid_NonFire_CPAD_Only$id <- seq.int(nrow(Grid_NonFire_CPAD_Only))
NonFire_CPAD_Only_iNat_shp <- NonFire_CPAD_Only_iNat_shp[Grid_NonFire_CPAD_Only, ]
NonFire_CPAD_Only_iNat_shp[is.na(NonFire_CPAD_Only_iNat_shp)] <- 0
st_write(NonFire_CPAD_Only_iNat_shp, "data/shp/iNat/NonFire/NonFire_CPAD_Only_iNat_shp.shp")
#mapview(NonFire_CPAD_Only_iNat_shp, map.type="OpenStreetMap")
#mapview(Grid_NonFire_CPAD_Only, map.type="OpenStreetMap")

# All
iNat_AllNonFire_CPAD_Only_Grids <- sf::st_join(Grid_NonFire_CPAD_Only,NonFire_CPAD_Only_iNat_shp)

iNat_AllNonFire_CPAD_Only_Grids <- iNat_AllNonFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_AllNonFire_CPAD_Only_Grids[is.na(iNat_AllNonFire_CPAD_Only_Grids)] <- 0

# Pre_NonFire
Pre_NonFire <- NonFire_CPAD_Only_iNat_shp %>% filter(month < 8)
iNat_PreNonFire_CPAD_Only_Grids <- sf::st_join(Grid_NonFire_CPAD_Only,Pre_NonFire)
rm(Pre_NonFire)

iNat_PreNonFire_CPAD_Only_Grids <- iNat_PreNonFire_CPAD_Only_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_PreNonFire_CPAD_Only_Grids[is.na(iNat_PreNonFire_CPAD_Only_Grids)] <- 0

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
