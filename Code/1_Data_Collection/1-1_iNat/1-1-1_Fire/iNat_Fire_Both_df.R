#Fire Both

# Libraries
library(tidyverse)
library(rinat)

# Define Months
Months <- c(1:12)
# Define Bounds
Fire_Both_Bounds <- c(39.51599,-121.4866,39.93372,-120.7468)

# Loop to access iNat Data
Fire_Both_iNat_list = list()
for (i in 1:12){
  tryCatch({
    # temp the get_inat_obs command from rinat
    temp = rinat::get_inat_obs(
      # use defined bounds
      bounds = Fire_Both_Bounds,
      # year of 2020, each loop is one month and put the max value to 10000
      year = 2020, month = Months[[i]], maxresults = 10000)
    # Rename temp file
    Fire_Both_iNat_list[[i]] = temp
    # Add an error function in case issue
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Read month 1
df1 <- Fire_Both_iNat_list[[1]]
# Read month 2
df2 <- Fire_Both_iNat_list[[2]]
# Read month 3
df3 <- Fire_Both_iNat_list[[3]]
# Read month 4
df4 <- Fire_Both_iNat_list[[4]]
# Read month 5
df5 <- Fire_Both_iNat_list[[5]]
# Read month 6
df6 <- Fire_Both_iNat_list[[6]]
# Read month 7
df7 <- Fire_Both_iNat_list[[7]]
# Read month 8
df8 <- Fire_Both_iNat_list[[8]]
# Read month 9
df9 <- Fire_Both_iNat_list[[9]]
# Read month 10
df10 <- Fire_Both_iNat_list[[10]]
# Read month 11
df11 <- Fire_Both_iNat_list[[11]]
# Read month 12
df12 <- Fire_Both_iNat_list[[12]]

# Combine all months into one variable
Fire_Both_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
# Remove temp variables
rm(Fire_Both_Bounds,Fire_Both_iNat_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# write a csv file
write.csv(Fire_Both_iNat_df,"data/csv/Fire_Both_iNat_df.csv")