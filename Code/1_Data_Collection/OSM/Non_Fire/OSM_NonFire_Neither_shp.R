library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(sf)
library(geojsonsf)

#NonFire Neither

NonFire_Neither_OSM_2019_1 = POST(
  "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
  encode = "form", 
  body = list(
    bboxes = "-121.449410,36.280496,-121.857879,36.555616", 
    time = "2019-12-01,2019-12-15", 
    showMetadata = "yes", properties = "metadata,tags"))

df1 <- httr::content(NonFire_Neither_OSM_2019_1, "text")
df1 <- grep("^/\\* [0-9]* \\*/", df1, value = TRUE, invert = TRUE)
n1 <- length(df1)
df1[-n1] <- gsub("^}$", "},", df1[-n1])
df1 <- c("[", df1, "]")
df1 <- jsonlite::flatten(fromJSON(df1))
df1 <- list(df1[[3]][[1]])
df1_type <- df1[[1]][["type"]]
df1_geom <- df1[[1]][["geometry"]]
df1_prop <- df1[[1]][["properties"]]
NonFire_Neither_OSM_2019_1 <- data.frame(df1_type, df1_geom, df1_prop)
rm(df1, df1_type,df1_geom,df1_prop)

NonFire_Neither_OSM_2019_2 = POST(
  "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
  encode = "form", 
  body = list(
    bboxes = "-121.449410,36.280496,-121.857879,36.555616", 
    time = "2019-12-16,2019-12-31", 
    showMetadata = "yes", properties = "metadata,tags"))

df1 <- httr::content(NonFire_Neither_OSM_2019_2, "text")
df1 <- grep("^/\\* [0-9]* \\*/", df1, value = TRUE, invert = TRUE)
n1 <- length(df1)
df1[-n1] <- gsub("^}$", "},", df1[-n1])
df1 <- c("[", df1, "]")
df1 <- jsonlite::flatten(fromJSON(df1))
df1 <- list(df1[[3]][[1]])
df1_type <- df1[[1]][["type"]]
df1_geom <- df1[[1]][["geometry"]]
df1_prop <- df1[[1]][["properties"]]
NonFire_Neither_OSM_2019_2 <- data.frame(df1_type, df1_geom, df1_prop)
rm(df1, df1_type,df1_geom,df1_prop)

NonFire_Neither_OSM_2019 <- rbind.fill(NonFire_Neither_OSM_2019_1,NonFire_Neither_OSM_2019_2)

rm(NonFire_Neither_OSM_2019_1,NonFire_Neither_OSM_2019_2)

osm_month <- c("2020-01-01,2020-01-31","2020-02-01,2020-02-29","2020-03-01,2020-03-31","2020-04-01,2020-04-30","2020-05-01,2020-05-31","2020-06-01,2020-06-30","2020-07-01,2020-07-31","2020-08-01,2020-08-31","2020-09-01,2020-09-30","2020-10-01,2020-10-31","2020-11-01,2020-11-30","2020-12-01,2020-12-31")

NonFire_Neither_OSM_list = list()
for (i in 1:12){
  tryCatch({
    temp = POST(
      "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
      encode = "form", 
      body = list(
        bboxes = "-121.449410,36.280496,-121.857879,36.555616", 
        time = osm_month[[i]], 
        showMetadata = "yes", properties = "metadata,tags"))
    NonFire_Neither_OSM_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- httr::content(NonFire_Neither_OSM_list[[1]], "text")
df1 <- geojsonsf::geojson_sf(df1)
df1 <- anti_join(df1, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df1 <- anti_join(df1, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df2 <- httr::content(NonFire_Neither_OSM_list[[2]], "text")
df2 <- geojsonsf::geojson_sf(df2)
df2 <- anti_join(df2, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df2 <- anti_join(df2, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df3 <- httr::content(NonFire_Neither_OSM_list[[3]], "text")
df3 <- geojsonsf::geojson_sf(df3)
df3 <- dplyr::anti_join(df3, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df3 <- anti_join(df3, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df4 <- httr::content(NonFire_Neither_OSM_list[[4]], "text")
df4 <- geojsonsf::geojson_sf(df4)
df4 <- anti_join(df4, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df4 <- anti_join(df4, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df5 <- httr::content(NonFire_Neither_OSM_list[[5]], "text")
df5 <- geojsonsf::geojson_sf(df5)
df5 <- anti_join(df5, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df5 <- anti_join(df5, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df6 <- httr::content(NonFire_Neither_OSM_list[[6]], "text")
df6 <- geojsonsf::geojson_sf(df6)
df6 <- anti_join(df6, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df6 <- anti_join(df6, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df7 <- httr::content(NonFire_Neither_OSM_list[[7]], "text")
df7 <- geojsonsf::geojson_sf(df7)
df7 <- anti_join(df7, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df7 <- anti_join(df7, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df8 <- httr::content(NonFire_Neither_OSM_list[[8]], "text")
df8 <- geojsonsf::geojson_sf(df8)
df8 <- anti_join(df8, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df8 <- anti_join(df8, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df9 <- httr::content(NonFire_Neither_OSM_list[[9]], "text")
df9 <- geojsonsf::geojson_sf(df9)
df9 <- anti_join(df9, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df9 <- anti_join(df9, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df10 <- httr::content(NonFire_Neither_OSM_list[[10]], "text")
df10 <- geojsonsf::geojson_sf(df10)
df10 <- anti_join(df10, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df10 <- anti_join(df10, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df11 <- httr::content(NonFire_Neither_OSM_list[[11]], "text")
df11 <- geojsonsf::geojson_sf(df11)
df11 <- anti_join(df11, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df11 <- anti_join(df11, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

df12 <- httr::content(NonFire_Neither_OSM_list[[12]], "text")
df12 <- geojsonsf::geojson_sf(df12)
df12 <- anti_join(df12, NonFire_Neither_OSM_2019, by = c("@osmId" = "X.osmId", "@version" = "X.version"))
df12 <- anti_join(df12, Fire_Neither_OSM_df, by = c("@osmId" = "X.osmId", "@version" = "X.version"))

NonFire_Neither_OSM_shp <- rbind.fill(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
NonFire_Neither_OSM_shp <- st_as_sf(NonFire_Neither_OSM_shp)

class(NonFire_Neither_OSM_shp)

rm(NonFire_Neither_OSM_2019,NonFire_Neither_OSM_list,df1,n1,df2,n2,df3,n3,df4,n4,df5,n5,df6,n6,df7,n7,df8,n8,df9,n9,df10,n10,df11,n11,df12,n12)