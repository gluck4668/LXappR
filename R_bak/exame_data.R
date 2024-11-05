
library(openxlsx)
library(dplyr)

rm(list=ls())

 df <- read.xlsx("dataset/nmr/lib_nmr_hmdb.xlsx")

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

 dd <- rbind(d0,int)

 dd <- distinct(dd,id,.keep_all = T)

 write.xlsx(int,"dataset/nmr/lib_nmr_hmdb_integer.xlsx",rownames=T)








