library(tidyverse)
library(sf)

# Neither
iNat_Cols <- c("id","iconic_taxon_name","datetime","longitude","latitude","positional_accuracy","quality_grade","Agreement_Score","DateDiff","month")
# NonFire Urban Only
NonFire_Neither_iNat_shp <- NonFire_Neither_iNat_df %>% 
  dplyr::select(iNat_Cols) %>% 
  tibble::as_tibble()
# Fill Blank with No_Name
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 72191606)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 38010009)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 37686941)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 51312214)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 40029708)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 39581640)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 44417678)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 43967494)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 43342333)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 43341442)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 41241027)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 47189040)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 47188863)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 47188750)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 46450852)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 46450810)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 50589869)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 52042724)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 60917566)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 60631313)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 59471973)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 58610854)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 75338439)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 63729016)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 63124834)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 63117677)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 63039258)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 62679837)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 65343774)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 64453833)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 66585205)] <- c("No_Name")
NonFire_Neither_iNat_shp$iconic_taxon_name[which(NonFire_Neither_iNat_shp$id == 66499984)] <- c("No_Name")


NonFire_Neither_iNat_shp$quality_grade[which(NonFire_Neither_iNat_shp$quality_grade == "research")] <- 1
NonFire_Neither_iNat_shp$quality_grade[which(NonFire_Neither_iNat_shp$quality_grade == "needs_id")] <- 0
NonFire_Neither_iNat_shp$quality_grade[which(NonFire_Neither_iNat_shp$quality_grade == "casual")] <- 0

NonFire_Neither_iNat_shp <- transform(NonFire_Neither_iNat_shp, quality_grade = as.numeric(quality_grade))
NonFire_Neither_iNat_shp <- transform(NonFire_Neither_iNat_shp, DateDiff = as.numeric(DateDiff))

# Pivot Wider
NonFire_Neither_iNat_shp <- NonFire_Neither_iNat_shp %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))

# Add Missing Data

# Write a csv as backup
write.csv(NonFire_Neither_iNat_shp,"data/shp/iNat/NonFire/csv/NonFire_Neither_iNat_shp.csv")
NonFire_Neither_iNat_shp <- st_as_sf(NonFire_Neither_iNat_shp, coords = c("longitude","latitude"))
st_crs(NonFire_Neither_iNat_shp)
NonFire_Neither_iNat_shp <- st_set_crs(NonFire_Neither_iNat_shp,"OGC:CRS84")
# Clip to NonFire Extent
Grid_NonFire_Neither <- 
  sf::st_read("data/Largest_Fires/New_Grids/Grid_NonFire_Neither.shp")
Grid_NonFire_Neither <- st_transform(Grid_NonFire_Neither, 4326)
Grid_NonFire_Neither$id <- seq.int(nrow(Grid_NonFire_Neither))
NonFire_Neither_iNat_shp <- NonFire_Neither_iNat_shp[Grid_NonFire_Neither, ]
NonFire_Neither_iNat_shp[is.na(NonFire_Neither_iNat_shp)] <- 0
st_write(NonFire_Neither_iNat_shp, "data/shp/iNat/NonFire/NonFire_Neither_iNat_shp.shp")
#mapview(NonFire_Neither_iNat_shp, map.type="OpenStreetMap")
#mapview(Grid_NonFire_Neither, map.type="OpenStreetMap")

# All
iNat_AllNonFire_Neither_Grids <- sf::st_join(Grid_NonFire_Neither,NonFire_Neither_iNat_shp)

iNat_AllNonFire_Neither_Grids <- iNat_AllNonFire_Neither_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_AllNonFire_Neither_Grids[is.na(iNat_AllNonFire_Neither_Grids)] <- 0

# Pre_NonFire
Pre_NonFire <- NonFire_Neither_iNat_shp %>% filter(month < 9)
iNat_PreNonFire_Neither_Grids <- sf::st_join(Grid_NonFire_Neither,Pre_NonFire)
rm(Pre_NonFire)

iNat_PreNonFire_Neither_Grids <- iNat_PreNonFire_Neither_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_PreNonFire_Neither_Grids[is.na(iNat_PreNonFire_Neither_Grids)] <- 0

# NonFire
NonFire <- NonFire_Neither_iNat_shp %>% filter(month == 9)
iNat_NonFire_Neither_Grids <- sf::st_join(Grid_NonFire_Neither,NonFire)
rm(NonFire)

iNat_NonFire_Neither_Grids <- iNat_NonFire_Neither_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_NonFire_Neither_Grids[is.na(iNat_NonFire_Neither_Grids)] <- 0

# Post_NonFire
Post_NonFire <- NonFire_Neither_iNat_shp %>% filter(month > 9)
iNat_PostNonFire_Neither_Grids <- sf::st_join(Grid_NonFire_Neither,Post_NonFire)
rm(Post_NonFire)

iNat_PostNonFire_Neither_Grids <- iNat_PostNonFire_Neither_Grids %>% select("id.x", "positional_accuracy", "quality_grade", "Agreement_Score", "DateDiff", "Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
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

iNat_PostNonFire_Neither_Grids[is.na(iNat_PostNonFire_Neither_Grids)] <- 0
