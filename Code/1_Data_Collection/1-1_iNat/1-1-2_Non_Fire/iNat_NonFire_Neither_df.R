#NonFire Neither

# Libraries
library(tidyverse)
library(rinat)

# Define Months
Months <- c(1:12)
# Define Bounds
NonFire_Neither_Bounds <- c(36.280496,-121.857879,36.555616,-121.449410)

# Loop to access iNat Data
NonFire_Neither_iNat_list = list()
for (i in 1:12){
  tryCatch({
    # temp the get_inat_obs command from rinat
    temp = rinat::get_inat_obs(
      # use defined bounds
      bounds = NonFire_Neither_Bounds,
      # year of 2020, each loop is one month and put the max value to 10000
      year = 2020, month = Months[[i]], maxresults = 10000)
    # Rename temp file
    NonFire_Neither_iNat_list[[i]] = temp
    # Add an error function in case issue
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Read month 1
df1 <- NonFire_Neither_iNat_list[[1]]
# Read month 2
df2 <- NonFire_Neither_iNat_list[[2]]
# Read month 3
df3 <- NonFire_Neither_iNat_list[[3]]
# Read month 4
df4 <- NonFire_Neither_iNat_list[[4]]
# Read month 5
df5 <- NonFire_Neither_iNat_list[[5]]
# Read month 6
df6 <- NonFire_Neither_iNat_list[[6]]
# Read month 7
df7 <- NonFire_Neither_iNat_list[[7]]
# Read month 8
df8 <- NonFire_Neither_iNat_list[[8]]
# Read month 9
df9 <- NonFire_Neither_iNat_list[[9]]
# Read month 10
df10 <- NonFire_Neither_iNat_list[[10]]
# Read month 11
df11 <- NonFire_Neither_iNat_list[[11]]
# Read month 12
df12 <- NonFire_Neither_iNat_list[[12]]

# Combine all months into one variable
NonFire_Neither_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
# Remove temp variables
rm(NonFire_Neither_Bounds,NonFire_Neither_iNat_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

NonFire_Neither_iNat_df$datetime <- as.Date(NonFire_Neither_iNat_df$datetime, format="%Y-%m-%d")
NonFire_Neither_iNat_df$month <- as.vector(month(NonFire_Neither_iNat_df$datetime))
NonFire_Neither_iNat_df <- anti_join(NonFire_Neither_iNat_df, Fire_Neither_iNat_df)

# write a csv file
write.csv(NonFire_Neither_iNat_df,"data/csv/NonFire_Neither_iNat_df.csv")