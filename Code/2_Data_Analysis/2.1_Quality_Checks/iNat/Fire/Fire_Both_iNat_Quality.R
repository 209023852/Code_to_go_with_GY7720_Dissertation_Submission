library(tidyverse)
library(lubridate)

## 3.1 iNaturalist Data
Fire_Both_iNat_df$datetime <- as.Date(Fire_Both_iNat_df$datetime, format="%Y-%m-%d")
Fire_Both_iNat_df$month <- as.vector(month(Fire_Both_iNat_df$datetime))

### 3.1.1 Spatial Accuracy

Mean_SA_PreFire <- Fire_Both_iNat_df %>% select(month, positional_accuracy) %>%
  filter(month < 8) %>%
  summarise(Mean_SA = mean(
    positional_accuracy, na.rm = T))

Mean_SA_Fire <- Fire_Both_iNat_df %>% select(month, positional_accuracy) %>%
  filter(month == 8)%>%
  summarise(Mean_SA = mean(
    positional_accuracy, na.rm = T))

Mean_SA_PostFire <- Fire_Both_iNat_df %>% select(month, positional_accuracy) %>%
  filter(month > 8)%>%
  summarise(Mean_SA = mean(
    positional_accuracy, na.rm = T))

Fire_Both_iNat_SA <- data.frame(
  Event_Period = c("iNat_Both_PreFire", "iNat_Both_Fire", "iNat_Both_PostFire"), 
  Mean_SA = c(Mean_SA_PreFire[[1]],Mean_SA_Fire[[1]],Mean_SA_PostFire[[1]]))

rm(Mean_SA_PreFire,Mean_SA_Fire,Mean_SA_PostFire)

### 3.1.2 Feature Level Accuracy
Fire_Both_iNat_df$Agreement_Score <- as.vector(Fire_Both_iNat_df$num_identification_agreements-Fire_Both_iNat_df$num_identification_disagreements)

Mean_FLA_PreFire <- Fire_Both_iNat_df %>% select(month, Agreement_Score) %>%
  filter(month < 8) %>%
  summarise(Mean_FLA = mean(
    Agreement_Score, na.rm = T))

Mean_FLA_Fire <- Fire_Both_iNat_df %>% select(month, Agreement_Score) %>%
  filter(month == 8)%>%
  summarise(Mean_FLA = mean(
    Agreement_Score, na.rm = T))

Mean_FLA_PostFire <- Fire_Both_iNat_df %>% select(month, Agreement_Score) %>%
  filter(month > 8)%>%
  summarise(Mean_FLA = mean(
    Agreement_Score, na.rm = T))

Fire_Both_iNat_FLA <- data.frame(
  Event_Period = c("iNat_Both_PreFire", "iNat_Both_Fire", "iNat_Both_PostFire"), 
  Mean_FLA = c(Mean_FLA_PreFire[[1]],Mean_FLA_Fire[[1]],Mean_FLA_PostFire[[1]]))

rm(Mean_FLA_PreFire,Mean_FLA_Fire,Mean_FLA_PostFire)

### 3.1.3 Completeness
Fire_Both_iNat_Comp <- Fire_Both_iNat_df %>%
  dplyr::group_by(month) %>%
  dplyr::count(month, quality_grade, sort=TRUE) %>%
  ungroup()

Fire_Both_iNat_Comp <- Fire_Both_iNat_Comp %>%
  dplyr::select(month, quality_grade, n) %>%
  dplyr::filter(quality_grade=="research") %>%
  dplyr::select(-quality_grade) %>%
  dplyr::arrange(month)
names(Fire_Both_iNat_Comp)[2] <- "Comp_By_Month"

Mean_Comp_PreFire <- Fire_Both_iNat_Comp %>% dplyr::select(month, Comp_By_Month) %>%
  dplyr::filter(month < 8) %>%
  dplyr::summarise(Mean_Comp = mean(
    Comp_By_Month, na.rm = T))

Mean_Comp_Fire <- Fire_Both_iNat_Comp %>% select(month, Comp_By_Month) %>%
  filter(month == 8) %>%
  summarise(Mean_Comp = mean(
    Comp_By_Month, na.rm = T))

Mean_Comp_PostFire <- Fire_Both_iNat_Comp %>%
  filter(month > 8) %>%
  summarise(Mean_Comp = mean(
    Comp_By_Month, na.rm = T))

Fire_Both_iNat_Comp <- data.frame(
  Event_Period = c("iNat_Both_PreFire", "iNat_Both_Fire", "iNat_Both_PostFire"), 
  Mean_Comp = c(Mean_Comp_PreFire[[1]],Mean_Comp_Fire[[1]],Mean_Comp_PostFire[[1]]))

rm(Mean_Comp_PreFire,Mean_Comp_Fire,Mean_Comp_PostFire)

### 3.1.4 Temporal Accuracy

#Date Difference
Fire_Both_iNat_df$DateDiff <- as.Date(as.character(ymd_hms(Fire_Both_iNat_df$created_at)))-as.Date(as.character(Fire_Both_iNat_df$datetime))

Mean_TA_PreFire <- Fire_Both_iNat_df %>% select(month, DateDiff) %>%
  filter(month < 8) %>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Mean_TA_Fire <- Fire_Both_iNat_df %>% select(month, DateDiff) %>%
  filter(month == 8) %>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Mean_TA_PostFire <- Fire_Both_iNat_df %>% select(month, DateDiff) %>%
  filter(month > 8) %>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Fire_Both_iNat_TA <- data.frame(
  Event_Period = c("iNat_Both_PreFire", "iNat_Both_Fire", "iNat_Both_PostFire"), 
  Mean_TA = c(Mean_TA_PreFire[[1]],Mean_TA_Fire[[1]],Mean_TA_PostFire[[1]]))

Fire_Both_iNat_TA <- transform(Fire_Both_iNat_TA, Both_Mean_TA = as.numeric(Both_Mean_TA))

rm(Mean_TA_PreFire,Mean_TA_Fire,Mean_TA_PostFire)

### 3.1.5 Taxon Count
Fire_Both_iNat_Taxon_Count <- Fire_Both_iNat_df %>% select(id, month, iconic_taxon_name)
Fire_Both_iNat_Taxon_Count$iconic_taxon_name[which(Fire_Both_iNat_Taxon_Count$id == 64246246)] <- c("No_Name")
Fire_Both_iNat_Taxon_Count$iconic_taxon_name[which(Fire_Both_iNat_Taxon_Count$id == 52311587)] <- c("No_Name")
Fire_Both_iNat_Taxon_Count$iconic_taxon_name[which(Fire_Both_iNat_Taxon_Count$id == 70913180)] <- c("No_Name")
Fire_Both_iNat_Taxon_Count$iconic_taxon_name[which(Fire_Both_iNat_Taxon_Count$id == 44748104)] <- c("No_Name")
Fire_Both_iNat_Taxon_Count$iconic_taxon_name[which(Fire_Both_iNat_Taxon_Count$id == 42258395)] <- c("No_Name")
Fire_Both_iNat_Taxon_Count <- Fire_Both_iNat_Taxon_Count %>% pivot_wider(names_from = iconic_taxon_name, values_from = iconic_taxon_name, values_fn = list(iconic_taxon_name = ~1), values_fill = list(iconic_taxon_name = 0))
Fire_Both_iNat_Taxon_Count <- Fire_Both_iNat_Taxon_Count %>% select(-id)
Fire_Both_iNat_Taxon_Count$Chromista <- 0

Fire_Both_iNat_Taxon_Count <- Fire_Both_iNat_Taxon_Count %>% select("month","Aves", "Plantae", "Fungi", "Animalia", "Insecta", "No_Name", "Arachnida", "Reptilia", "Mammalia", "Mollusca", "Chromista", "Amphibia", "Actinopterygii", "Protozoa") %>%
  group_by(month) %>%
  summarise(
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
    protozoa_count = sum(Protozoa, na.rm = F)
  ) %>%
  arrange(month)

Taxon_Count_PreFire <- Fire_Both_iNat_Taxon_Count %>%
  filter(month < 8) %>%
  summarise(
    PreFire_Mean_plantae_count = mean(plantae_count, na.rm = F),
    PreFire_Mean_fungi_count = mean(fungi_count, na.rm = F),
    PreFire_Mean_animalia_count = mean(animalia_count, na.rm = F),
    PreFire_Mean_insecta_count = mean(insecta_count, na.rm = F),
    PreFire_Mean_other_count = mean(other_count, na.rm = F),
    PreFire_Mean_arachnida_count = mean(arachnida_count, na.rm = F),
    PreFire_Mean_reptilia_count = mean(reptilia_count, na.rm = F),
    PreFire_Mean_mammalia_count = mean(mammalia_count, na.rm = F),
    PreFire_Mean_mollusca_count = mean(mollusca_count, na.rm = F),
    PreFire_Mean_chromista_count = mean(chromista_count, na.rm = F),
    PreFire_Mean_amphibia_count = mean(amphibia_count, na.rm = F),
    PreFire_Mean_actinopterygii_count = mean(actinopterygii_count, na.rm = F),
    PreFire_Mean_protozoa_count = mean(protozoa_count, na.rm = F)
  )

Taxon_Count_Fire <- Fire_Both_iNat_Taxon_Count %>%
  filter(month == 8) %>%
  summarise(
    Fire_Mean_plantae_count = mean(plantae_count, na.rm = F),
    Fire_Mean_fungi_count = mean(fungi_count, na.rm = F),
    Fire_Mean_animalia_count = mean(animalia_count, na.rm = F),
    Fire_Mean_insecta_count = mean(insecta_count, na.rm = F),
    Fire_Mean_other_count = mean(other_count, na.rm = F),
    Fire_Mean_arachnida_count = mean(arachnida_count, na.rm = F),
    Fire_Mean_reptilia_count = mean(reptilia_count, na.rm = F),
    Fire_Mean_mammalia_count = mean(mammalia_count, na.rm = F),
    Fire_Mean_mollusca_count = mean(mollusca_count, na.rm = F),
    Fire_Mean_chromista_count = mean(chromista_count, na.rm = F),
    Fire_Mean_amphibia_count = mean(amphibia_count, na.rm = F),
    Fire_Mean_actinopterygii_count = mean(actinopterygii_count, na.rm = F),
    Fire_Mean_protozoa_count = mean(protozoa_count, na.rm = F)
  )

Taxon_Count_PostFire <- Fire_Both_iNat_Taxon_Count %>%
  filter(month > 8) %>%
  summarise(
    PostFire_Mean_plantae_count = mean(plantae_count, na.rm = F),
    PostFire_Mean_fungi_count = mean(fungi_count, na.rm = F),
    PostFire_Mean_animalia_count = mean(animalia_count, na.rm = F),
    PostFire_Mean_insecta_count = mean(insecta_count, na.rm = F),
    PostFire_Mean_other_count = mean(other_count, na.rm = F),
    PostFire_Mean_arachnida_count = mean(arachnida_count, na.rm = F),
    PostFire_Mean_reptilia_count = mean(reptilia_count, na.rm = F),
    PostFire_Mean_mammalia_count = mean(mammalia_count, na.rm = F),
    PostFire_Mean_mollusca_count = mean(mollusca_count, na.rm = F),
    PostFire_Mean_chromista_count = mean(chromista_count, na.rm = F),
    PostFire_Mean_amphibia_count = mean(amphibia_count, na.rm = F),
    PostFire_Mean_actinopterygii_count = mean(actinopterygii_count, na.rm = F),
    PostFire_Mean_protozoa_count = mean(protozoa_count, na.rm = F)
  )

Fire_Both_iNat_Taxon_Count <- data.frame(
  Event_Period = c("iNat_Both_PreFire", "iNat_Both_Fire", "iNat_Both_PostFire"), 
  Mean_plantae_count = c(
    Taxon_Count_PreFire[[1]],
    Taxon_Count_Fire[[1]],
    Taxon_Count_PostFire[[1]]),
  Mean_fungi_count = c(
    Taxon_Count_PreFire[[2]],
    Taxon_Count_Fire[[2]],
    Taxon_Count_PostFire[[2]]),
  Mean_animalia_count = c(
    Taxon_Count_PreFire[[3]],
    Taxon_Count_Fire[[3]],
    Taxon_Count_PostFire[[3]]),
  Mean_insecta_count = c(
    Taxon_Count_PreFire[[4]],
    Taxon_Count_Fire[[4]],
    Taxon_Count_PostFire[[4]]),
  Mean_other_count = c(
    Taxon_Count_PreFire[[5]],
    Taxon_Count_Fire[[5]],
    Taxon_Count_PostFire[[5]]),
  Mean_arachnida_count = c(
    Taxon_Count_PreFire[[6]],
    Taxon_Count_Fire[[6]],
    Taxon_Count_PostFire[[6]]),
  Mean_reptilia_count = c(
    Taxon_Count_PreFire[[7]],
    Taxon_Count_Fire[[7]],
    Taxon_Count_PostFire[[7]]),
  Mean_mammalia_count = c(
    Taxon_Count_PreFire[[8]],
    Taxon_Count_Fire[[8]],
    Taxon_Count_PostFire[[8]]),
  Mean_mollusca_count = c(
    Taxon_Count_PreFire[[9]],
    Taxon_Count_Fire[[9]],
    Taxon_Count_PostFire[[9]]),
  Mean_chromista_count = c(
    Taxon_Count_PreFire[[10]],
    Taxon_Count_Fire[[10]],
    Taxon_Count_PostFire[[10]]),
  Mean_amphibia_count = c(
    Taxon_Count_PreFire[[11]],
    Taxon_Count_Fire[[11]],
    Taxon_Count_PostFire[[11]]),
  Mean_actinopterygii_count = c(
    Taxon_Count_PreFire[[12]],
    Taxon_Count_Fire[[12]],
    Taxon_Count_PostFire[[12]]),
  Mean_protozoa_count = c(
    Taxon_Count_PreFire[[13]],
    Taxon_Count_Fire[[13]],
    Taxon_Count_PostFire[[13]])
)

rm(Taxon_Count_PreFire,Taxon_Count_Fire,Taxon_Count_PostFire)

### 3.1.6 Final Table Join

Fire_Both_iNat_Quality <- left_join(Fire_Both_iNat_SA, Fire_Both_iNat_FLA, by="Event_Period")
Fire_Both_iNat_Quality <- left_join(Fire_Both_iNat_Quality, Fire_Both_iNat_Comp, by="Event_Period")
Fire_Both_iNat_Quality <- left_join(Fire_Both_iNat_Quality, Fire_Both_iNat_TA, by="Event_Period")
Fire_Both_iNat_Quality <- left_join(Fire_Both_iNat_Quality, Fire_Both_iNat_Taxon_Count, by="Event_Period")

rm(Fire_Both_iNat_SA, Fire_Both_iNat_FLA,Fire_Both_iNat_Comp,Fire_Both_iNat_TA,Fire_Both_iNat_Taxon_Count)

Fire_Both_iNat_Quality

write.csv(Fire_Both_iNat_Quality, "Analysis/Quality_Loops/csv/Fire_Both_iNat_Quality.csv")