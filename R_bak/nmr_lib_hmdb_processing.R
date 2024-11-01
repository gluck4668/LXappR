
library(data.table)
library(dplyr)
library(stringr)
library(foreach)
library(plyr)
library(openxlsx)

rm(list=ls())

lib_fun <- function() {

lib_list <- dir("hmdb_nmr_peak_lists",full.names = T)

lib_list <- lib_list[which(!grepl("two",lib_list,ignore.case = T))]

set <- data.frame()

#----------------------------------------
x=1

foreach(x=c(1:length(lib_list))) %do% {

print(paste0("Number ",x, " of ", length(lib_list)," is being processed..."))

file_ename <- str_extract(lib_list[x],"(?<=\\/).*?(?=_)" )

df <- fread(lib_list[x]) %>% data.frame()

if( !any(grepl("F2ppm",names(df),ignore.case = T)) ){
  df <- df[,c(2)] %>% data.frame()
  df <- t(df) %>% data.frame()
  rownames(df) <- file_ename

if(x==1)
  set <- rbind(set,df) else
  {
    if ( ncol(df)==ncol(set) )
      set <- rbind(set,df) else
        {
        if(ncol(df) < ncol(set)){
          n <- ncol(set)-ncol(df)
          df <- t(df) %>% data.frame()

          s=nrow(df)+1
          e=nrow(df)+n
          df[c(s:e),] <- NA
          df <- t(df) %>% data.frame()
          set <- rbind(set,df)
        } else
        {
          n <- ncol(df)-ncol(set)
          set <- t(set) %>% data.frame()

          s=nrow(set)+1
          e=nrow(set)+n
          set[c(s:e),] <- NA

          set <- t(set) %>% data.frame()
          set <- rbind(set,df)

        }


      }

  }

#colnames(set) <- paste0("v",c(1:ncol(set)))
#colnames(df) <- paste0("v",c(1:ncol(df)))

}

} # foreach x end

colnames(set) <- paste0("v",c(1:ncol(set)))
set_sub <- subset(set, !is.na(v2))
set_sub <- subset(set_sub,!grepl("ppm",v1))

colnames(set_sub) <- paste0("shift",c(1:ncol(set_sub)))

write.xlsx(set_sub,"lib_nmr_hmdb.xlsx",rowNames=T)

}

lib_fun()



