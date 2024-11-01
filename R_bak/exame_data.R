
library(openxlsx)
library(dplyr)

rm(list=ls())

 df <- read.xlsx("D:/Desktop/LXappR 2024-11-01/dataset/nmr/lib_nmr_hmdb.xlsx")

 d0 <- data.frame()

 int <- data.frame()

 i=1

 for(i in 1:nrow(df)){

   set <- df[i,] %>% .[!is.na(.)] %>% .[-1] %>% as.numeric()

   if(any(set==0))
      d0 <- rbind(d0,df[i,])

   if( all(set %% 1 ==0) )
     int <- rbind(int,df[i,])

 }

 write.xlsx(d0,"D:/Desktop/LXappR/dataset/nmr/lib_nmr_hmdb_d0.xlsx",rownames=T)
 write.xlsx(int,"D:/Desktop/LXappR/dataset/nmr/lib_nmr_hmdb_integer.xlsx",rownames=T)







