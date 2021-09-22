library(sf)
library(tidyverse)

Fire_Both_Feature_Type <- c("aeroway","amenity","barrier","boundary","building","highway","historic","landuse","leisure","man_made","natural","place","power","railway","route","shop","sport","tourism","water","waterway")
OSM_Cols <- c("@osmId","@version",Fire_Both_Feature_Type)

Fire_Both_OSM_shp <- Fire_Both_OSM_shp %>% select(OSM_Cols)

Fire_Both_OSM_shp$DateDiff <- Fire_Both_OSM_df$DateDiff
Fire_Both_OSM_shp$NACount <- Fire_Both_OSM_df$NACount
Fire_Both_OSM_shp$month <- Fire_Both_OSM_df$month
Fire_Both_OSM_shp <- Fire_Both_OSM_shp %>% mutate_at(vars(-c("@osmId","@version","DateDiff","NACount","month","geometry")), ~replace(., !is.na(.), 1))
Fire_Both_OSM_shp[is.na(Fire_Both_OSM_shp)] <- 0

# Add Missing Data
Fire_Both_OSM_shp$emergency <- 0
Fire_Both_OSM_shp$office <- 0
Fire_Both_OSM_shp$railway <- 0

Fire_Both_OSM_shp <- st_transform(Fire_Both_OSM_shp, 4326)
# Clip to Fire Extent
Grid_Fire_Both <- 
  sf::st_read("data/Largest_Fires/New_Grids/Grid_Fire_Both.shp")
Grid_Fire_Both <- st_transform(Grid_Fire_Both, 4326)
Fire_Both_OSM_shp <- Fire_Both_OSM_shp[Grid_Fire_Both, ]
Fire_Both_OSM_shp[is.na(Fire_Both_OSM_shp)] <- 0
st_write(Fire_Both_OSM_shp, "data/shp/OSM/Fire/Fire_Both_OSM_shp.shp")

# Set All Columns to Numeric
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, NACount = as.numeric(NACount))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, DateDiff = as.numeric(DateDiff))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, aeroway = as.numeric(aeroway))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, amenity = as.numeric(amenity))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, barrier = as.numeric(barrier))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, boundary = as.numeric(boundary))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, building = as.numeric(building))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, emergency = as.numeric(emergency))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, highway = as.numeric(highway))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, historic = as.numeric(historic))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, landuse = as.numeric(landuse))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, leisure = as.numeric(leisure))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, man_made = as.numeric(man_made))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, natural = as.numeric(natural))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, office = as.numeric(office))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, place = as.numeric(place))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, power = as.numeric(power))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, railway = as.numeric(railway))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, route = as.numeric(route))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, shop = as.numeric(shop))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, sport = as.numeric(sport))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, tourism = as.numeric(tourism))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, water = as.numeric(water))
Fire_Both_OSM_shp <- transform(Fire_Both_OSM_shp, waterway = as.numeric(waterway))

# Pre_Fire
Pre_Fire <- Fire_Both_OSM_shp %>% filter(month < 8)
OSM_PreFire_Both_Grids <- sf::st_join(Grid_Fire_Both,Pre_Fire)
rm(Pre_Fire)

OSM_PreFire_Both_Grids <- OSM_PreFire_Both_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
  group_by(id) %>%
  summarise(
    avg_version = mean(X.version, na.rm = F),
    avg_NACount = mean(NACount, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
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
    waterway_count = sum(waterway, na.rm = F),
    feature_count = sum(aeroway,amenity,barrier,boundary,building,emergency,highway,historic,landuse,leisure,man_made,natural,office,place,power,railway,route,shop,sport,tourism,water,waterway, na.rm = F)
  ) %>%
  arrange(id)

OSM_PreFire_Both_Grids[is.na(OSM_PreFire_Both_Grids)] <- 0

# Fire
Fire <- Fire_Both_OSM_shp %>% filter(month == 8)
OSM_Fire_Both_Grids <- sf::st_join(Grid_Fire_Both,Fire)
rm(Fire)

OSM_Fire_Both_Grids <- OSM_Fire_Both_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
  group_by(id) %>%
  summarise(
    avg_version = mean(X.version, na.rm = F),
    avg_NACount = mean(NACount, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
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
    waterway_count = sum(waterway, na.rm = F),
    feature_count = sum(aeroway,amenity,barrier,boundary,building,emergency,highway,historic,landuse,leisure,man_made,natural,office,place,power,railway,route,shop,sport,tourism,water,waterway, na.rm = F)
  ) %>%
  arrange(id)

OSM_Fire_Both_Grids[is.na(OSM_Fire_Both_Grids)] <- 0

# Post_Fire
Post_Fire <- Fire_Both_OSM_shp %>% filter(month > 8)
OSM_PostFire_Both_Grids <- sf::st_join(Grid_Fire_Both,Post_Fire)
rm(Post_Fire)

OSM_PostFire_Both_Grids <- OSM_PostFire_Both_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
  group_by(id) %>%
  summarise(
    avg_version = mean(X.version, na.rm = F),
    avg_NACount = mean(NACount, na.rm = F),
    avg_datediff = mean(DateDiff, na.rm = F),
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
    waterway_count = sum(waterway, na.rm = F),
    feature_count = sum(aeroway,amenity,barrier,boundary,building,emergency,highway,historic,landuse,leisure,man_made,natural,office,place,power,railway,route,shop,sport,tourism,water,waterway, na.rm = F)
  ) %>%
  arrange(id)

OSM_PostFire_Both_Grids[is.na(OSM_PostFire_Both_Grids)] <- 0