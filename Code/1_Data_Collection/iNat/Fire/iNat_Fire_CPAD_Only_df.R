library(tidyverse)
library(rinat)
#Fire CPAD Only
Fire_CPAD_Only_Bounds <- c(39.43731,-123.2783,40.123,-122.5431)
Months <- c(1:12)

Fire_CPAD_Only_iNat_list = list()
for (i in 1:12){
  tryCatch({
    temp = rinat::get_inat_obs(
      bounds = Fire_CPAD_Only_Bounds,
      year = 2020, month = Months[[i]], maxresults = 10000)
    Fire_CPAD_Only_iNat_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- Fire_CPAD_Only_iNat_list[[1]]

df2 <- Fire_CPAD_Only_iNat_list[[2]]

df3 <- Fire_CPAD_Only_iNat_list[[3]]

df4 <- Fire_CPAD_Only_iNat_list[[4]]

df5 <- Fire_CPAD_Only_iNat_list[[5]]

df6 <- Fire_CPAD_Only_iNat_list[[6]]

df7 <- Fire_CPAD_Only_iNat_list[[7]]

df8 <- Fire_CPAD_Only_iNat_list[[8]]

df9 <- Fire_CPAD_Only_iNat_list[[9]]

df10 <- Fire_CPAD_Only_iNat_list[[10]]

df11 <- Fire_CPAD_Only_iNat_list[[11]]

df12 <- Fire_CPAD_Only_iNat_list[[12]]

Fire_CPAD_Only_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

rm(Fire_CPAD_Only_Bounds,Fire_CPAD_Only_iNat_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Remove rows that cause issues
#Fire_CPAD_Only_iNat_df <- Fire_CPAD_Only_iNat_df[-c(71389,50641,40550,40833,61003,72990,80180,81251,81329,81332,81915,92745,116244,50621,50626,50642,71390), ] 
#Fire_CPAD_Only_iNat_df <- subset(Fire_CPAD_Only_iNat_df, grepl('^\\d+$', Fire_CPAD_Only_iNat_df$num_identification_agreements))
#Fire_CPAD_Only_iNat_df <- Fire_CPAD_Only_iNat_df %>% subset(!id=="straybird726")
#Fire_CPAD_Only_iNat_df <- transform(Fire_CPAD_Only_iNat_df, num_identification_agreements = as.numeric(num_identification_agreements))
#Fire_CPAD_Only_iNat_df <- transform(Fire_CPAD_Only_iNat_df, num_identification_disagreements = as.numeric(num_identification_disagreements))
#Fire_CPAD_Only_iNat_df <- transform(Fire_CPAD_Only_iNat_df, positional_accuracy = as.numeric(positional_accuracy))

write.csv(Fire_CPAD_Only_iNat_df,"data/csv/Fire_CPAD_Only_iNat_df.csv")
