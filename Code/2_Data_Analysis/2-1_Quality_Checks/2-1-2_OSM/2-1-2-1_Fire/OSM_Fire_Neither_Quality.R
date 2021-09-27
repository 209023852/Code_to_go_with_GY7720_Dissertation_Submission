library(tidyverse)
library(lubridate)

Fire_Neither_OSM_df$month <- as.Date(as.character(ymd_hms(Fire_Neither_OSM_df$X.validFrom)))
Fire_Neither_OSM_df$month <- month(Fire_Neither_OSM_df$month)

### 3.2.1 Spatial Accuracy
Fire_Neither_OSM_SA <- Fire_Neither_OSM_df %>%
  select(month, X.osmId, X.version) %>%
  group_by(month)

Fire_Neither_OSM_SA_Neither <-subset(Fire_Neither_OSM_SA,ave(1:nrow(Fire_Neither_OSM_SA),X.osmId,X.version, FUN = length)>1)
Fire_Neither_OSM_SA_ID <-subset(Fire_Neither_OSM_SA,ave(1:nrow(Fire_Neither_OSM_SA),X.osmId, FUN = length)>1)
Fire_Neither_OSM_SA_Anti <- anti_join(Fire_Neither_OSM_SA_ID, Fire_Neither_OSM_SA_Neither, by=c("X.osmId","X.version"))

Fire_Neither_OSM_SA <- Fire_Neither_OSM_SA_Anti %>%
  group_by(month) %>%
  count(month, X.osmId, sort=TRUE)

rm(Fire_Neither_OSM_SA_Neither, Fire_Neither_OSM_SA_ID, Fire_Neither_OSM_SA_Anti)

Fire_Neither_OSM_SA <- Fire_Neither_OSM_SA %>%
  select(month, n) %>%
  group_by(month) %>%
  summarise(Frequency = sum(n))
names(Fire_Neither_OSM_SA)[2] <- "Total_Changes_By_Month"

Mean_SA_PreFire <- Fire_Neither_OSM_SA %>% select(month, Total_Changes_By_Month) %>%
  filter(month < 9) %>%
  summarise(Mean_SA = mean(
    Total_Changes_By_Month, na.rm = T))

Mean_SA_Fire <- Fire_Neither_OSM_SA %>% select(month, Total_Changes_By_Month) %>%
  filter(month == 9)%>%
  summarise(Mean_SA = mean(
    Total_Changes_By_Month, na.rm = T))

Mean_SA_PostFire <- Fire_Neither_OSM_SA %>% select(month, Total_Changes_By_Month) %>%
  filter(month > 9)%>%
  summarise(Mean_SA = mean(
    Total_Changes_By_Month, na.rm = T))

Fire_Neither_OSM_SA <- data.frame(
  Event_Period = c("OSM_Neither_PreFire", "OSM_Neither_Fire", "OSM_Neither_PostFire"),  
  Mean_SA = c(Mean_SA_PreFire[[1]],Mean_SA_Fire[[1]],Mean_SA_PostFire[[1]]))

rm(Mean_SA_PreFire,Mean_SA_Fire,Mean_SA_PostFire)

### 3.2.2 Feature Level Accuracy

Mean_FLA_PreFire <- Fire_Neither_OSM_df %>% select(month, X.version) %>%
  filter(month < 9) %>%
  summarise(Mean_FLA = mean(
    X.version, na.rm = T))

Mean_FLA_Fire <- Fire_Neither_OSM_df %>% select(month, X.version) %>%
  filter(month == 9)%>%
  summarise(Mean_FLA = mean(
    X.version, na.rm = T))

Mean_FLA_PostFire <- Fire_Neither_OSM_df %>% select(month, X.version) %>%
  filter(month > 9)%>%
  summarise(Mean_FLA = mean(
    X.version, na.rm = T))

Fire_Neither_OSM_FLA <- data.frame(
  Event_Period = c("OSM_Neither_PreFire", "OSM_Neither_Fire", "OSM_Neither_PostFire"),  
  Mean_FLA = c(Mean_FLA_PreFire[[1]],Mean_FLA_Fire[[1]],Mean_FLA_PostFire[[1]]))

rm(Mean_FLA_PreFire,Mean_FLA_Fire,Mean_FLA_PostFire)

### 3.2.3 Completeness
Fire_Neither_OSM_df$NACount <- rowSums(is.na(Fire_Neither_OSM_df))

Mean_Comp_PreFire <- Fire_Neither_OSM_df %>% select(month, NACount) %>%
  filter(month < 9) %>%
  summarise(Mean_Comp = mean(
    NACount, na.rm = T))

Mean_Comp_Fire <- Fire_Neither_OSM_df %>% select(month, NACount) %>%
  filter(month == 9)%>%
  summarise(Mean_Comp = mean(
    NACount, na.rm = T))

Mean_Comp_PostFire <- Fire_Neither_OSM_df %>% select(month, NACount) %>%
  filter(month > 9)%>%
  summarise(Mean_Comp = mean(
    NACount, na.rm = T))

Fire_Neither_OSM_Comp <- data.frame(
  Event_Period = c("OSM_Neither_PreFire", "OSM_Neither_Fire", "OSM_Neither_PostFire"), 
  Mean_Comp = c(Mean_Comp_PreFire[[1]],Mean_Comp_Fire[[1]],Mean_Comp_PostFire[[1]]))

rm(Mean_Comp_PreFire,Mean_Comp_Fire,Mean_Comp_PostFire)

### 3.1.4 Temporal Accuracy
#Date Difference
Fire_Neither_OSM_df$DateDiff <- as.Date(as.character(ymd_hms(Fire_Neither_OSM_df$X.validTo)))-as.Date(as.character(Fire_Neither_OSM_df$X.validFrom))

Mean_TA_PreFire <- Fire_Neither_OSM_df %>% select(month, DateDiff) %>%
  filter(month < 9) %>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Mean_TA_Fire <- Fire_Neither_OSM_df %>% select(month, DateDiff) %>%
  filter(month == 9)%>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Mean_TA_PostFire <- Fire_Neither_OSM_df %>% select(month, DateDiff) %>%
  filter(month > 9)%>%
  summarise(Mean_TA = mean(
    DateDiff, na.rm = T))

Fire_Neither_OSM_TA <- data.frame(
  Event_Period = c("OSM_Neither_PreFire", "OSM_Neither_Fire", "OSM_Neither_PostFire"),  
  Mean_TA = c(Mean_TA_PreFire[[1]],Mean_TA_Fire[[1]],Mean_TA_PostFire[[1]]))

Fire_Neither_OSM_TA <- transform(Fire_Neither_OSM_TA, Mean_TA = as.numeric(Mean_TA))

rm(Mean_TA_PreFire,Mean_TA_Fire,Mean_TA_PostFire)

### 3.2.4 Feature_Count

Fire_Neither_OSM_Feature_Count <- Fire_Neither_OSM_df

Fire_Neither_OSM_Feature_Count$emergency <- NA
Fire_Neither_OSM_Feature_Count$historic <- NA
Fire_Neither_OSM_Feature_Count$office <- NA
Fire_Neither_OSM_Feature_Count$power <- NA
Fire_Neither_OSM_Feature_Count$railway <- NA
Fire_Neither_OSM_Feature_Count$shop <- NA

Fire_Neither_OSM_Feature_Count <- Fire_Neither_OSM_Feature_Count %>%
  select("month","aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office",
         "place","power","railway","route","shop","sport","tourism","water","waterway"
  )

Fire_Neither_OSM_Feature_Count <- Fire_Neither_OSM_Feature_Count %>% mutate_at(vars(-c(month)), ~replace(., !is.na(.), 1))
Fire_Neither_OSM_Feature_Count[is.na(Fire_Neither_OSM_Feature_Count)] <- 0

Fire_Neither_OSM_Feature_Count <- Fire_Neither_OSM_Feature_Count %>% mutate_if(is.character,as.numeric)

Fire_Neither_OSM_Feature_Count <- Fire_Neither_OSM_Feature_Count %>% 
  group_by(month) %>%
  summarise(
    aeroway_count = sum(aeroway, na.rm = F),
    amenity_count = sum(amenity, na.rm = F),
    barrier_count = sum(barrier, na.rm = F),
    boundary_count = sum(boundary, na.rm = F),
    building_count = sum(building, na.rm = F),
    emergency_count = sum(emergency, na.rm = F),
    highway_count = sum(highway, na.rm = F),
    historic_count = sum(historic, na.rm = F),
    landuse_count = sum(landuse, na.rm = F),
    leisure_count = sum(leisure, na.rm = F),
    man_made_count = sum(man_made, na.rm = F),
    natural_count = sum(natural, na.rm = F),
    office_count = sum(office, na.rm = F),
    place_count = sum(place, na.rm = F),
    power_count = sum(power, na.rm = F),
    railway_count = sum(railway, na.rm = F),
    route_count = sum(route, na.rm = F),
    shop_count = sum(shop, na.rm = F),
    sport_count = sum(sport, na.rm = F),    
    tourism_count = sum(tourism, na.rm = F),
    water_count = sum(water, na.rm = F),
    waterway_count = sum(waterway, na.rm = F)
  ) %>%
  arrange(month)

Feature_Count_PreFire <- Fire_Neither_OSM_Feature_Count %>%
  filter(month < 9) %>%
  summarise(
    PreFire_aeroway_count = mean(aeroway_count, na.rm = F),
    PreFire_amenity_count = mean(amenity_count, na.rm = F),
    PreFire_barrier_count = mean(barrier_count, na.rm = F),
    PreFire_boundary_count = mean(boundary_count, na.rm = F),
    PreFire_building_count = mean(building_count, na.rm = F),
    PreFire_emergency_count = mean(emergency_count, na.rm = F),
    PreFire_highway_count = mean(highway_count, na.rm = F),
    PreFire_historic_count = mean(historic_count, na.rm = F),
    PreFire_landuse_count = mean(landuse_count, na.rm = F),
    PreFire_leisure_count = mean(leisure_count, na.rm = F),
    PreFire_man_made_count = mean(man_made_count, na.rm = F),
    PreFire_natural_count = mean(natural_count, na.rm = F),
    PreFire_office_count = mean(office_count, na.rm = F),
    PreFire_place_count = mean(place_count, na.rm = F),
    PreFire_power_count = mean(power_count, na.rm = F),
    PreFire_railway_count = mean(railway_count, na.rm = F),
    PreFire_route_count = mean(route_count, na.rm = F),
    PreFire_shop_count = mean(shop_count, na.rm = F),
    PreFire_sport_count = mean(sport_count, na.rm = F),    
    PreFire_tourism_count = mean(tourism_count, na.rm = F),
    PreFire_water_count = mean(water_count, na.rm = F),
    PreFire_waterway_count = mean(waterway_count, na.rm = F)
  )

Feature_Count_Fire <- Fire_Neither_OSM_Feature_Count %>%
  filter(month == 9) %>%
  summarise(
    Fire_aeroway_count = mean(aeroway_count, na.rm = F),
    Fire_amenity_count = mean(amenity_count, na.rm = F),
    Fire_barrier_count = mean(barrier_count, na.rm = F),
    Fire_boundary_count = mean(boundary_count, na.rm = F),
    Fire_building_count = mean(building_count, na.rm = F),
    Fire_emergency_count = mean(emergency_count, na.rm = F),
    Fire_highway_count = mean(highway_count, na.rm = F),
    Fire_historic_count = mean(historic_count, na.rm = F),
    Fire_landuse_count = mean(landuse_count, na.rm = F),
    Fire_leisure_count = mean(leisure_count, na.rm = F),
    Fire_man_made_count = mean(man_made_count, na.rm = F),
    Fire_natural_count = mean(natural_count, na.rm = F),
    Fire_office_count = mean(office_count, na.rm = F),
    Fire_place_count = mean(place_count, na.rm = F),
    Fire_power_count = mean(power_count, na.rm = F),
    Fire_railway_count = mean(railway_count, na.rm = F),
    Fire_route_count = mean(route_count, na.rm = F),
    Fire_shop_count = mean(shop_count, na.rm = F),
    Fire_sport_count = mean(sport_count, na.rm = F),    
    Fire_tourism_count = mean(tourism_count, na.rm = F),
    Fire_water_count = mean(water_count, na.rm = F),
    Fire_waterway_count = mean(waterway_count, na.rm = F)
  )

Feature_Count_PostFire <- Fire_Neither_OSM_Feature_Count %>%
  filter(month > 9) %>%
  summarise(
    PostFire_aeroway_count = mean(aeroway_count, na.rm = F),
    PostFire_amenity_count = mean(amenity_count, na.rm = F),
    PostFire_barrier_count = mean(barrier_count, na.rm = F),
    PostFire_boundary_count = mean(boundary_count, na.rm = F),
    PostFire_building_count = mean(building_count, na.rm = F),
    PostFire_emergency_count = mean(emergency_count, na.rm = F),
    PostFire_highway_count = mean(highway_count, na.rm = F),
    PostFire_historic_count = mean(historic_count, na.rm = F),
    PostFire_landuse_count = mean(landuse_count, na.rm = F),
    PostFire_leisure_count = mean(leisure_count, na.rm = F),
    PostFire_man_made_count = mean(man_made_count, na.rm = F),
    PostFire_natural_count = mean(natural_count, na.rm = F),
    PostFire_office_count = mean(office_count, na.rm = F),
    PostFire_place_count = mean(place_count, na.rm = F),
    PostFire_power_count = mean(power_count, na.rm = F),
    PostFire_railway_count = mean(railway_count, na.rm = F),
    PostFire_route_count = mean(route_count, na.rm = F),
    PostFire_shop_count = mean(shop_count, na.rm = F),
    PostFire_sport_count = mean(sport_count, na.rm = F),    
    PostFire_tourism_count = mean(tourism_count, na.rm = F),
    PostFire_water_count = mean(water_count, na.rm = F),
    PostFire_waterway_count = mean(waterway_count, na.rm = F)
  )

Fire_Neither_OSM_Feature_Count <- data.frame(
  Event_Period = c("OSM_Neither_PreFire", "OSM_Neither_Fire", "OSM_Neither_PostFire"), 
  Mean_aeroway_count = c(
    Feature_Count_PreFire[[1]],
    Feature_Count_Fire[[1]],
    Feature_Count_PostFire[[1]]),
  Mean_amenity_count = c(
    Feature_Count_PreFire[[2]],
    Feature_Count_Fire[[2]],
    Feature_Count_PostFire[[2]]),
  Mean_barrier_count = c(
    Feature_Count_PreFire[[3]],
    Feature_Count_Fire[[3]],
    Feature_Count_PostFire[[3]]),
  Mean_boundary_count = c(
    Feature_Count_PreFire[[4]],
    Feature_Count_Fire[[4]],
    Feature_Count_PostFire[[4]]),
  Mean_building_count = c(
    Feature_Count_PreFire[[5]],
    Feature_Count_Fire[[5]],
    Feature_Count_PostFire[[5]]),
  Mean_emergency_count = c(
    Feature_Count_PreFire[[6]],
    Feature_Count_Fire[[6]],
    Feature_Count_PostFire[[6]]),
  Mean_highway_count = c(
    Feature_Count_PreFire[[7]],
    Feature_Count_Fire[[7]],
    Feature_Count_PostFire[[7]]),
  Mean_historic_count = c(
    Feature_Count_PreFire[[8]],
    Feature_Count_Fire[[8]],
    Feature_Count_PostFire[[8]]),
  Mean_landuse_count = c(
    Feature_Count_PreFire[[9]],
    Feature_Count_Fire[[9]],
    Feature_Count_PostFire[[9]]),
  Mean_leisure_count = c(
    Feature_Count_PreFire[[10]],
    Feature_Count_Fire[[10]],
    Feature_Count_PostFire[[10]]),
  Mean_man_made_count = c(
    Feature_Count_PreFire[[11]],
    Feature_Count_Fire[[11]],
    Feature_Count_PostFire[[11]]),
  Mean_natural_count = c(
    Feature_Count_PreFire[[12]],
    Feature_Count_Fire[[12]],
    Feature_Count_PostFire[[12]]),
  Mean_office_count = c(
    Feature_Count_PreFire[[13]],
    Feature_Count_Fire[[13]],
    Feature_Count_PostFire[[13]]),
  Mean_place_count = c(
    Feature_Count_PreFire[[14]],
    Feature_Count_Fire[[14]],
    Feature_Count_PostFire[[14]]),
  Mean_power_count = c(
    Feature_Count_PreFire[[15]],
    Feature_Count_Fire[[15]],
    Feature_Count_PostFire[[15]]),
  Mean_railway_count = c(
    Feature_Count_PreFire[[16]],
    Feature_Count_Fire[[16]],
    Feature_Count_PostFire[[16]]),
  Mean_route_count = c(
    Feature_Count_PreFire[[17]],
    Feature_Count_Fire[[17]],
    Feature_Count_PostFire[[17]]),
  Mean_shop_count = c(
    Feature_Count_PreFire[[18]],
    Feature_Count_Fire[[18]],
    Feature_Count_PostFire[[18]]),
  Mean_sport_count = c(
    Feature_Count_PreFire[[19]],
    Feature_Count_Fire[[19]],
    Feature_Count_PostFire[[19]]),
  Mean_tourism_count = c(
    Feature_Count_PreFire[[20]],
    Feature_Count_Fire[[20]],
    Feature_Count_PostFire[[20]]),
  Mean_water_count = c(
    Feature_Count_PreFire[[21]],
    Feature_Count_Fire[[21]],
    Feature_Count_PostFire[[21]]),
  Mean_waterway_count = c(
    Feature_Count_PreFire[[22]],
    Feature_Count_Fire[[22]],
    Feature_Count_PostFire[[22]])
)

rm(Feature_Count_PreFire,Feature_Count_Fire,Feature_Count_PostFire)

### 3.1.6 Final Table Join

Fire_Neither_OSM_Quality <- left_join(Fire_Neither_OSM_SA, Fire_Neither_OSM_FLA, by="Event_Period")
Fire_Neither_OSM_Quality <- left_join(Fire_Neither_OSM_Quality, Fire_Neither_OSM_Comp, by="Event_Period")
Fire_Neither_OSM_Quality <- left_join(Fire_Neither_OSM_Quality, Fire_Neither_OSM_TA, by="Event_Period")
Fire_Neither_OSM_Quality <- left_join(Fire_Neither_OSM_Quality, Fire_Neither_OSM_Feature_Count, by="Event_Period")

rm(Fire_Neither_OSM_SA, Fire_Neither_OSM_FLA,Fire_Neither_OSM_Comp,Fire_Neither_OSM_TA,Fire_Neither_OSM_Feature_Count)

Fire_Neither_OSM_Quality

write.csv(Fire_Neither_OSM_Quality, "Analysis/Quality_Loops/csv/Fire_Neither_OSM_Quality.csv")