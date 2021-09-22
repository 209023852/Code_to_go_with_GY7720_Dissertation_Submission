library(tidyverse)
library(rinat)
#Fire Both
Months <- c(1:12)
Fire_Both_Bounds <- c(39.51599,-121.4866,39.93372,-120.7468)

Fire_Both_iNat_list = list()
for (i in 1:12){
  tryCatch({
    temp = rinat::get_inat_obs(
      bounds = Fire_Both_Bounds,
      year = 2020, month = Months[[i]], maxresults = 10000)
    Fire_Both_iNat_list[[i]] = temp
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1 <- Fire_Both_iNat_list[[1]]

df2 <- Fire_Both_iNat_list[[2]]

df3 <- Fire_Both_iNat_list[[3]]

df4 <- Fire_Both_iNat_list[[4]]

df5 <- Fire_Both_iNat_list[[5]]

df6 <- Fire_Both_iNat_list[[6]]

df7 <- Fire_Both_iNat_list[[7]]

df8 <- Fire_Both_iNat_list[[8]]

df9 <- Fire_Both_iNat_list[[9]]

df10 <- Fire_Both_iNat_list[[10]]

df11 <- Fire_Both_iNat_list[[11]]

df12 <- Fire_Both_iNat_list[[12]]

Fire_Both_iNat_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

rm(Fire_Both_Bounds,Fire_Both_iNat_list,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Remove rows that cause issues
#Fire_Both_iNat_df <- subset(Fire_Both_iNat_df, grepl('^\\d+$', Fire_Both_iNat_df$num_identification_agreements))
#Fire_Both_iNat_df <- Fire_Both_iNat_df %>% subset(!id=="aaabbbxxx")
#Fire_Both_iNat_df <- Fire_Both_iNat_df %>% subset(!id=="straybird726")
#Fire_Both_iNat_df <- transform(Fire_Both_iNat_df, num_identification_agreements = as.numeric(num_identification_agreements))
#Fire_Both_iNat_df <- transform(Fire_Both_iNat_df, num_identification_disagreements = as.numeric(num_identification_disagreements))
#Fire_Both_iNat_df <- transform(Fire_Both_iNat_df, positional_accuracy = as.numeric(positional_accuracy))
write.csv(Fire_Both_iNat_df,"data/csv/Fire_Both_iNat_df.csv")