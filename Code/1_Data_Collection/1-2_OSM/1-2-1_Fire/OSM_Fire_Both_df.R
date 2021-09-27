#Fire Both

# Libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)

# Find 2019 data
# Loop the API query 
Fire_Both_OSM_2019_1 = POST(
  # location is full history ohsome API
  "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
  encode = "form", 
  body = list(
    # Define extents
    bboxes = "-120.74681,39.51599,-121.48662,39.93372", 
    # Define time
    time = "2019-12-01,2019-12-15", 
    # Show all meta data
    showMetadata = "yes", properties = "metadata,tags"))

# Extract the content as text
df1 <- content(Fire_Both_OSM_2019_1, "text")
# tidy the text
df1 <- grep("^/\\* [0-9]* \\*/", df1, value = TRUE, invert = TRUE)
n1 <- length(df1)
df1[-n1] <- gsub("^}$", "},", df1[-n1])
df1 <- c("[", df1, "]")
# Convert from JSON
df1 <- jsonlite::flatten(fromJSON(df1))
# Extract the content type, geometry and properties
df1 <- list(df1[[3]][[1]])
df1_type <- df1[[1]][["type"]]
df1_geom <- df1[[1]][["geometry"]]
df1_prop <- df1[[1]][["properties"]]
# Combine all extracted data
Fire_Both_OSM_2019_1 <- data.frame(df1_type, df1_geom, df1_prop)
# Remove all temp files
rm(df1, df1_type,df1_geom,df1_prop)

# Repeat for other dates in 2019 and 2020 data

Fire_Both_OSM_2019_2 = POST(
  "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
  encode = "form", 
  body = list(
    bboxes = "-120.74681,39.51599,-121.48662,39.93372", 
    time = "2019-12-16,2019-12-31", 
    showMetadata = "yes", properties = "metadata,tags"))

df1 <- content(Fire_Both_OSM_2019_2, "text")
df1 <- grep("^/\\* [0-9]* \\*/", df1, value = TRUE, invert = TRUE)
n1 <- length(df1)
df1[-n1] <- gsub("^}$", "},", df1[-n1])
df1 <- c("[", df1, "]")
df1 <- jsonlite::flatten(fromJSON(df1))
df1 <- list(df1[[3]][[1]])
df1_type <- df1[[1]][["type"]]
df1_geom <- df1[[1]][["geometry"]]
df1_prop <- df1[[1]][["properties"]]
Fire_Both_OSM_2019_2 <- data.frame(df1_type, df1_geom, df1_prop)
rm(df1, df1_type,df1_geom,df1_prop)

# Make 1 2019 file
Fire_Both_OSM_2019 <- rbind.fill(Fire_Both_OSM_2019_1,Fire_Both_OSM_2019_2)
# Remove temp files
rm(Fire_Both_OSM_2019_1,Fire_Both_OSM_2019_2)

# Find 2020 data
# Define months
osm_month <- c("2020-01-01,2020-01-31","2020-02-01,2020-02-29","2020-03-01,2020-03-31","2020-04-01,2020-04-30","2020-05-01,2020-05-31","2020-06-01,2020-06-30","2020-07-01,2020-07-31","2020-08-01,2020-08-31","2020-09-01,2020-09-30","2020-10-01,2020-10-31","2020-11-01,2020-11-30","2020-12-01,2020-12-31")

Fire_Both_OSM_list = list()
for (i in 1:12){
  tryCatch({
    temp = POST(
      "https://api.ohsome.org/v1/elementsFullHistory/geometry", 
      encode = "form", 
      body = list(
        bboxes = "-120.74681,39.51599,-121.48662,39.93372", 
        time = osm_month[[i]], 
        showMetadata = "yes", properties = "metadata,tags"))
    Fire_Both_OSM_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#rm(Fire_Both_Bounds_OSM)

df1 <- content(Fire_Both_OSM_list[[1]], "text")
df1 <- grep("^/\\* [0-9]* \\*/", df1, value = TRUE, invert = TRUE)
n1 <- length(df1)
df1[-n1] <- gsub("^}$", "},", df1[-n1])
df1 <- c("[", df1, "]")
df1 <- jsonlite::flatten(fromJSON(df1))
df1 <- list(df1[[3]][[1]])
df1_type <- df1[[1]][["type"]]
df1_geom <- df1[[1]][["geometry"]]
df1_prop <- df1[[1]][["properties"]]
df1 <- data.frame(df1_type, df1_geom, df1_prop)
# remove 2019 data
df1 <- anti_join(df1, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df1_type,df1_geom,df1_prop)

df2 <- content(Fire_Both_OSM_list[[2]], "text")
df2 <- grep("^/\\* [0-9]* \\*/", df2, value = TRUE, invert = TRUE)
n2 <- length(df2)
df2[-n2] <- gsub("^}$", "},", df2[-n2])
df2 <- c("[", df2, "]")
df2 <- jsonlite::flatten(fromJSON(df2))
df2 <- list(df2[[3]][[1]])
df2_type <- df2[[1]][["type"]]
df2_geom <- df2[[1]][["geometry"]]
df2_prop <- df2[[1]][["properties"]]
df2 <- data.frame(df2_type, df2_geom, df2_prop)
df2 <- anti_join(df2, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df2_type,df2_geom,df2_prop)

df3 <- content(Fire_Both_OSM_list[[3]], "text")
df3 <- grep("^/\\* [0-9]* \\*/", df3, value = TRUE, invert = TRUE)
n3 <- length(df3)
df3[-n3] <- gsub("^}$", "},", df3[-n3])
df3 <- c("[", df3, "]")
df3 <- jsonlite::flatten(fromJSON(df3))
df3 <- list(df3[[3]][[1]])
df3_type <- df3[[1]][["type"]]
df3_geom <- df3[[1]][["geometry"]]
df3_prop <- df3[[1]][["properties"]]
df3 <- data.frame(df3_type, df3_geom, df3_prop)
df3 <- anti_join(df3, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df3_type,df3_geom,df3_prop)

df4 <- content(Fire_Both_OSM_list[[4]], "text")
df4 <- grep("^/\\* [0-9]* \\*/", df4, value = TRUE, invert = TRUE)
n4 <- length(df4)
df4[-n4] <- gsub("^}$", "},", df4[-n4])
df4 <- c("[", df4, "]")
df4 <- jsonlite::flatten(fromJSON(df4))
df4 <- list(df4[[3]][[1]])
df4_type <- df4[[1]][["type"]]
df4_geom <- df4[[1]][["geometry"]]
df4_prop <- df4[[1]][["properties"]]
df4 <- data.frame(df4_type, df4_geom, df4_prop)
df4 <- anti_join(df4, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df4_type,df4_geom,df4_prop)

df5 <- content(Fire_Both_OSM_list[[5]], "text")
df5 <- grep("^/\\* [0-9]* \\*/", df5, value = TRUE, invert = TRUE)
n5 <- length(df5)
df5[-n5] <- gsub("^}$", "},", df5[-n5])
df5 <- c("[", df5, "]")
df5 <- jsonlite::flatten(fromJSON(df5))
df5 <- list(df5[[3]][[1]])
df5_type <- df5[[1]][["type"]]
df5_geom <- df5[[1]][["geometry"]]
df5_prop <- df5[[1]][["properties"]]
df5 <- data.frame(df5_type, df5_geom, df5_prop)
df5 <- anti_join(df5, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df5_type,df5_geom,df5_prop)

df6 <- content(Fire_Both_OSM_list[[6]], "text")
df6 <- grep("^/\\* [0-9]* \\*/", df6, value = TRUE, invert = TRUE)
n6 <- length(df6)
df6[-n6] <- gsub("^}$", "},", df6[-n6])
df6 <- c("[", df6, "]")
df6 <- jsonlite::flatten(fromJSON(df6))
df6 <- list(df6[[3]][[1]])
df6_type <- df6[[1]][["type"]]
df6_geom <- df6[[1]][["geometry"]]
df6_prop <- df6[[1]][["properties"]]
df6 <- data.frame(df6_type, df6_geom, df6_prop)
df6 <- anti_join(df6, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df6_type,df6_geom,df6_prop)

df7 <- content(Fire_Both_OSM_list[[7]], "text")
df7 <- grep("^/\\* [0-9]* \\*/", df7, value = TRUE, invert = TRUE)
n7 <- length(df7)
df7[-n7] <- gsub("^}$", "},", df7[-n7])
df7 <- c("[", df7, "]")
df7 <- jsonlite::flatten(fromJSON(df7))
df7 <- list(df7[[3]][[1]])
df7_type <- df7[[1]][["type"]]
df7_geom <- df7[[1]][["geometry"]]
df7_prop <- df7[[1]][["properties"]]
df7 <- data.frame(df7_type, df7_geom, df7_prop)
df7 <- anti_join(df7, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df7_type,df7_geom,df7_prop)

df8 <- content(Fire_Both_OSM_list[[8]], "text")
df8 <- grep("^/\\* [0-9]* \\*/", df8, value = TRUE, invert = TRUE)
n8 <- length(df8)
df8[-n8] <- gsub("^}$", "},", df8[-n8])
df8 <- c("[", df8, "]")
df8 <- jsonlite::flatten(fromJSON(df8))
df8 <- list(df8[[3]][[1]])
df8_type <- df8[[1]][["type"]]
df8_geom <- df8[[1]][["geometry"]]
df8_prop <- df8[[1]][["properties"]]
df8 <- data.frame(df8_type, df8_geom, df8_prop)
df8 <- anti_join(df8, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df8_type,df8_geom,df8_prop)

df9 <- content(Fire_Both_OSM_list[[9]], "text")
df9 <- grep("^/\\* [0-9]* \\*/", df9, value = TRUE, invert = TRUE)
n9 <- length(df9)
df9[-n9] <- gsub("^}$", "},", df9[-n9])
df9 <- c("[", df9, "]")
df9 <- jsonlite::flatten(fromJSON(df9))
df9 <- list(df9[[3]][[1]])
df9_type <- df9[[1]][["type"]]
df9_geom <- df9[[1]][["geometry"]]
df9_prop <- df9[[1]][["properties"]]
df9 <- data.frame(df9_type, df9_geom, df9_prop)
df9 <- anti_join(df9, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df9_type,df9_geom,df9_prop)

df10 <- content(Fire_Both_OSM_list[[10]], "text")
df10 <- grep("^/\\* [0-9]* \\*/", df10, value = TRUE, invert = TRUE)
n10 <- length(df10)
df10[-n10] <- gsub("^}$", "},", df10[-n10])
df10 <- c("[", df10, "]")
df10 <- jsonlite::flatten(fromJSON(df10))
df10 <- list(df10[[3]][[1]])
df10_type <- df10[[1]][["type"]]
df10_geom <- df10[[1]][["geometry"]]
df10_prop <- df10[[1]][["properties"]]
df10 <- data.frame(df10_type, df10_geom, df10_prop)
df10 <- anti_join(df10, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df10_type,df10_geom,df10_prop)

df11 <- content(Fire_Both_OSM_list[[11]], "text")
df11 <- grep("^/\\* [0-9]* \\*/", df11, value = TRUE, invert = TRUE)
n11 <- length(df11)
df11[-n11] <- gsub("^}$", "},", df11[-n11])
df11 <- c("[", df11, "]")
df11 <- jsonlite::flatten(fromJSON(df11))
df11 <- list(df11[[3]][[1]])
df11_type <- df11[[1]][["type"]]
df11_geom <- df11[[1]][["geometry"]]
df11_prop <- df11[[1]][["properties"]]
df11 <- data.frame(df11_type, df11_geom, df11_prop)
df11 <- anti_join(df11, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df11_type,df11_geom,df11_prop)

df12 <- content(Fire_Both_OSM_list[[12]], "text")
df12 <- grep("^/\\* [0-9]* \\*/", df12, value = TRUE, invert = TRUE)
n12 <- length(df12)
df12[-n12] <- gsub("^}$", "},", df12[-n12])
df12 <- c("[", df12, "]")
df12 <- jsonlite::flatten(fromJSON(df12))
df12 <- list(df12[[3]][[1]])
df12_type <- df12[[1]][["type"]]
df12_geom <- df12[[1]][["geometry"]]
df12_prop <- df12[[1]][["properties"]]
df12 <- data.frame(df12_type, df12_geom, df12_prop)
df12 <- anti_join(df12, Fire_Both_OSM_2019, by = c("X.osmId" = "X.osmId", "X.version" = "X.version"))
rm(df12_type,df12_geom,df12_prop)

# make a singular 2020 file
Fire_Both_OSM_df <- rbind.fill(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# remove temp files
rm(Fire_Both_OSM_2019,Fire_Both_OSM_list,df1,n1,df2,n2,df3,n3,df4,n4,df5,n5,df6,n6,df7,n7,df8,n8,df9,n9,df10,n10,df11,n11,df12,n12)
#write.csv(Fire_Both_OSM_df,"data/csv/Fire_Both_OSM_df.csv")
