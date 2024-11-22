library(openxlsx)

lib_nmr_hmdb <- read.csv ("lib_nmr_hmdb.csv")
nmr_400M <- readRDS("nmr_400M.rds")

usethis::use_data(lib_nmr_hmdb,overwrite = T)
usethis::use_data(nmr_400M,overwrite = T)

data(lib_nmr_hmdb)
