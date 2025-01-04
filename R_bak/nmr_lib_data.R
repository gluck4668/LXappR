
library(openxlsx)
library(dplyr)

rm(list=ls())

hmdb_id <- readRDS("dataset/nmr/hmdb_id.rds")
nmr_hmdb <- readRDS("dataset/nmr/nmr_hmdb.rds")
Chenomx_400M <- readRDS("dataset/nmr/Chenomx_400M.rds")
Chenomx_500M <- readRDS("dataset/nmr/Chenomx_500M.rds")
Chenomx_600M <- readRDS("dataset/nmr/Chenomx_600M.rds")

usethis::use_data(hmdb_id,overwrite = T)
usethis::use_data(nmr_hmdb,overwrite = T)
usethis::use_data(Chenomx_400M,overwrite = T)
usethis::use_data(Chenomx_500M,overwrite = T)
usethis::use_data(Chenomx_600M,overwrite = T)

data(hmdb_id)
data(nmr_hmdb)
data(Chenomx_400M)
data(Chenomx_500M)
data(Chenomx_600M)


# Chenomx_600M <- read.xlsx("dataset/nmr/Chenomx_600M.xlsx")
# saveRDS(Chenomx_600M,"dataset/nmr/Chenomx_600M.rds")
