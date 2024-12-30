
library(openxlsx)
library(dplyr)


hmdb_id <- readRDS("dataset/nmr/hmdb_id.rds")
nmr_hmdb <- readRDS("dataset/nmr/nmr_hmdb.rds")
nmr_400M <- readRDS("dataset/nmr/nmr_400M.rds")
nmr_500M <- readRDS("dataset/nmr/nmr_500M.rds")

usethis::use_data(hmdb_id,overwrite = T)
usethis::use_data(nmr_hmdb,overwrite = T)
usethis::use_data(nmr_400M,overwrite = T)
usethis::use_data(nmr_500M,overwrite = T)

data(hmdb_id)
data(nmr_hmdb)
data(nmr_400M)
data(nmr_500M)


# hmdb <- read.xlsx("dataset/nmr/lib_nmr_hmdb 2024-12-30.xlsx")
# saveRDS(hmdb,"dataset/nmr/nmr_hmdb.rds")
