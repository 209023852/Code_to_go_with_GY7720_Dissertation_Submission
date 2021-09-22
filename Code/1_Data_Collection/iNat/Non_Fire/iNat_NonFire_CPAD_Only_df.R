library(tidyverse)
library(rinat)
library(lubridate)
#NonFire CPAD_Only
Fire_CPAD_Only_County_Bounds <- c(39.311863,-123.433407,40.284458,-122.377541)
Months <- c(1:12)

Fire_CPAD_Only_iNat_County_list = list()
for (i in 1:12){
  tryCatch({
    temp = rinat::get_inat_obs(
      bounds = Fire_CPAD_Only_County_Bounds,
      year = 2020, month = Months[[i]], maxresults = 10000)
    Fire_CPAD_Only_iNat_County_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- Fire_CPAD_Only_iNat_County_list[[1]]

df2 <- Fire_CPAD_Only_iNat_County_list[[2]]

df3 <- Fire_CPAD_Only_iNat_County_list[[3]]

df4 <- Fire_CPAD_Only_iNat_County_list[[4]]

df5 <- Fire_CPAD_Only_iNat_County_list[[5]]

df6 <- Fire_CPAD_Only_iNat_County_list[[6]]

df7 <- Fire_CPAD_Only_iNat_County_list[[7]]

df8 <- Fire_CPAD_Only_iNat_County_list[[8]]

df9 <- Fire_CPAD_Only_iNat_County_list[[9]]

df10 <- Fire_CPAD_Only_iNat_County_list[[10]]

df11 <- Fire_CPAD_Only_iNat_County_list[[11]]

df12 <- Fire_CPAD_Only_iNat_County_list[[12]]

Fire_CPAD_Only_County_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

rm(Fire_CPAD_Only_County_Bounds,Fire_CPAD_Only_iNat_County_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

Fire_CPAD_Only_County_iNat_df$datetime <- as.Date(Fire_CPAD_Only_County_iNat_df$datetime, format="%Y-%m-%d")
Fire_CPAD_Only_County_iNat_df$month <- as.vector(month(Fire_CPAD_Only_County_iNat_df$datetime))

NonFire_CPAD_Only_iNat_df <- anti_join(Fire_CPAD_Only_County_iNat_df, Fire_CPAD_Only_iNat_df)

write.csv(NonFire_CPAD_Only_iNat_df,"data/csv/NonFire_CPAD_Only_iNat_df.csv")

rm(Fire_CPAD_Only_County_iNat_df)