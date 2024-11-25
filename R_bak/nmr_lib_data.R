
library(openxlsx)
library(dplyr)


hmdb_id <- readRDS("hmdb_id.rds")
nmr_hmdb <- readRDS("nmr_hmdb.rds")
nmr_400M <- readRDS("nmr_400M.rds")
nmr_500M <- readRDS("nmr_500M.rds")

usethis::use_data(hmdb_id,overwrite = T)
usethis::use_data(nmr_hmdb,overwrite = T)
usethis::use_data(nmr_400M,overwrite = T)
usethis::use_data(nmr_500M,overwrite = T)

data(hmdb_id)
data(nmr_hmdb)
data(nmr_400M)
data(nmr_500M)


