# Urban_Only Grids

# Libraries
library(sf)
library(tidyverse)

# define features
NonFire_Urban_Only_Feature_Type <- c("amenity","barrier","boundary","building","emergency","highway","landuse","leisure","man_made","natural","office","place","route","shop","sport","tourism","water","waterway")
OSM_Cols <- c("@osmId","@version",NonFire_Urban_Only_Feature_Type)

# Extract only features
NonFire_Urban_Only_OSM_shp <- NonFire_Urban_Only_OSM_shp %>% select(OSM_Cols)

# Add Quality Checks
NonFire_Urban_Only_OSM_shp$DateDiff <- NonFire_Urban_Only_OSM_df$DateDiff
NonFire_Urban_Only_OSM_shp$NACount <- NonFire_Urban_Only_OSM_df$NACount
NonFire_Urban_Only_OSM_shp$month <- NonFire_Urban_Only_OSM_df$month
NonFire_Urban_Only_OSM_shp <- NonFire_Urban_Only_OSM_shp %>% mutate_at(vars(-c("@osmId","@version","DateDiff","NACount","month","geometry")), ~replace(., !is.na(.), 1))
NonFire_Urban_Only_OSM_shp[is.na(NonFire_Urban_Only_OSM_shp)] <- 0

# Add Missing Data
NonFire_Urban_Only_OSM_shp$aeroway <- 0
NonFire_Urban_Only_OSM_shp$historic <- 0
NonFire_Urban_Only_OSM_shp$power <- 0
NonFire_Urban_Only_OSM_shp$railway <- 0
# Make sure it is in WGS84
NonFire_Urban_Only_OSM_shp <- st_transform(NonFire_Urban_Only_OSM_shp, 4326)

# Read grids file
Grid_NonFire_Urban_Only <- 
  sf::st_read("data/Largest_NonFires/New_Grids/Grid_NonFire_Urban_Only.shp")
# Make sure it is in WGS84
Grid_NonFire_Urban_Only <- st_transform(Grid_NonFire_Urban_Only, 4326)
# Clip to NonFire Extent
NonFire_Urban_Only_OSM_shp <- NonFire_Urban_Only_OSM_shp[Grid_NonFire_Urban_Only, ]
# make all NA values 0
NonFire_Urban_Only_OSM_shp[is.na(NonFire_Urban_Only_OSM_shp)] <- 0
st_write(NonFire_Urban_Only_OSM_shp, "data/shp/OSM/NonFire/NonFire_Urban_Only_OSM_shp.shp")

# Set All Columns to Numeric
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, NACount = as.numeric(NACount))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, DateDiff = as.numeric(DateDiff))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, aeroway = as.numeric(aeroway))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, amenity = as.numeric(amenity))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, barrier = as.numeric(barrier))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, boundary = as.numeric(boundary))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, building = as.numeric(building))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, emergency = as.numeric(emergency))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, highway = as.numeric(highway))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, historic = as.numeric(historic))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, landuse = as.numeric(landuse))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, leisure = as.numeric(leisure))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, man_made = as.numeric(man_made))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, natural = as.numeric(natural))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, office = as.numeric(office))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, place = as.numeric(place))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, power = as.numeric(power))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, railway = as.numeric(railway))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, route = as.numeric(route))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, shop = as.numeric(shop))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, sport = as.numeric(sport))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, tourism = as.numeric(tourism))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, water = as.numeric(water))
NonFire_Urban_Only_OSM_shp <- transform(NonFire_Urban_Only_OSM_shp, waterway = as.numeric(waterway))

# Pre_NonFire
# Filter to months prior to NonFire
Pre_NonFire <- NonFire_Urban_Only_OSM_shp %>% filter(month < 6)
# Join the data
OSM_PreNonFire_Urban_Only_Grids <- sf::st_join(Grid_NonFire_Urban_Only,Pre_NonFire)
# Remove temp files
rm(Pre_NonFire)

# Select key columns
OSM_PreNonFire_Urban_Only_Grids <- OSM_PreNonFire_Urban_Only_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
  # group by id
  group_by(id) %>%
  # find mean of all quality checks and sum of features
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

# Make NA values 0
OSM_PreNonFire_Urban_Only_Grids[is.na(OSM_PreNonFire_Urban_Only_Grids)] <- 0

# NonFire
NonFire <- NonFire_Urban_Only_OSM_shp %>% filter(month == 6)
OSM_NonFire_Urban_Only_Grids <- sf::st_join(Grid_NonFire_Urban_Only,NonFire)
rm(NonFire)

OSM_NonFire_Urban_Only_Grids <- OSM_NonFire_Urban_Only_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
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

OSM_NonFire_Urban_Only_Grids[is.na(OSM_NonFire_Urban_Only_Grids)] <- 0

# Post_NonFire
Post_NonFire <- NonFire_Urban_Only_OSM_shp %>% filter(month > 6)
OSM_PostNonFire_Urban_Only_Grids <- sf::st_join(Grid_NonFire_Urban_Only,Post_NonFire)
rm(Post_NonFire)

OSM_PostNonFire_Urban_Only_Grids <- OSM_PostNonFire_Urban_Only_Grids %>% select("id", "X.version","NACount", "DateDiff", "aeroway","amenity","barrier","boundary","building","emergency","highway","historic","landuse","leisure","man_made","natural","office","place","power","railway","route","shop","sport","tourism","water","waterway") %>%
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

OSM_PostNonFire_Urban_Only_Grids[is.na(OSM_PostNonFire_Urban_Only_Grids)] <- 0