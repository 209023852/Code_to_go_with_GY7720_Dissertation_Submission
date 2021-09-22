library(tidyverse)
library(rinat)
#Fire Neither
Months <- c(1:12)
Fire_Neither_Bounds <- c(36.39062,-121.718,36.45139,-121.5867)

Fire_Neither_iNat_list = list()
for (i in 1:12){
  tryCatch({
    temp = rinat::get_inat_obs(
      bounds = Fire_Neither_Bounds,
      year = 2020, month = Months[[i]], maxresults = 10000)
    Fire_Neither_iNat_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- Fire_Neither_iNat_list[[1]]

df2 <- Fire_Neither_iNat_list[[2]]

df3 <- Fire_Neither_iNat_list[[3]]

df4 <- Fire_Neither_iNat_list[[4]]

df5 <- Fire_Neither_iNat_list[[5]]

df6 <- Fire_Neither_iNat_list[[6]]

df7 <- Fire_Neither_iNat_list[[7]]

df8 <- Fire_Neither_iNat_list[[8]]

df9 <- Fire_Neither_iNat_list[[9]]

df10 <- Fire_Neither_iNat_list[[10]]

df11 <- Fire_Neither_iNat_list[[11]]

df12 <- Fire_Neither_iNat_list[[12]]

Fire_Neither_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

rm(Fire_Neither_Bounds,Fire_Neither_iNat_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

write.csv(Fire_Neither_iNat_df,"data/csv/Fire_Neither_iNat_df.csv")
