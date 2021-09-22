library(tidyverse)
library(rinat)
library(lubridate)
#NonFire Neither
Fire_Neither_County_Bounds <- c(36.280496,-121.857879,36.555616,-121.449410)
Months <- c(1:12)

Fire_Neither_iNat_County_list = list()
for (i in 1:12){
  tryCatch({
    temp = rinat::get_inat_obs(
      bounds = Fire_Neither_County_Bounds,
      year = 2020, month = Months[[i]], maxresults = 10000)
    Fire_Neither_iNat_County_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- Fire_Neither_iNat_County_list[[1]]

df2 <- Fire_Neither_iNat_County_list[[2]]

df3 <- Fire_Neither_iNat_County_list[[3]]

df4 <- Fire_Neither_iNat_County_list[[4]]

df5 <- Fire_Neither_iNat_County_list[[5]]

df6 <- Fire_Neither_iNat_County_list[[6]]

df7 <- Fire_Neither_iNat_County_list[[7]]

df8 <- Fire_Neither_iNat_County_list[[8]]

df9 <- Fire_Neither_iNat_County_list[[9]]

df10 <- Fire_Neither_iNat_County_list[[10]]

df11 <- Fire_Neither_iNat_County_list[[11]]

df12 <- Fire_Neither_iNat_County_list[[12]]

Fire_Neither_County_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

rm(Fire_Neither_County_Bounds,Fire_Neither_iNat_County_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

Fire_Neither_County_iNat_df$datetime <- as.Date(Fire_Neither_County_iNat_df$datetime, format="%Y-%m-%d")
Fire_Neither_County_iNat_df$month <- as.vector(month(Fire_Neither_County_iNat_df$datetime))

NonFire_Neither_iNat_df <- anti_join(Fire_Neither_County_iNat_df, Fire_Neither_iNat_df)

write.csv(NonFire_Neither_iNat_df,"data/csv/NonFire_Neither_iNat_df.csv")

rm(Fire_Neither_County_iNat_df)
