library(openxlsx)

nmr_hmdb <- readRDS("nmr_hmdb.rds")
nmr_400M <- readRDS("nmr_400M.rds")

usethis::use_data(nmr_hmdb,overwrite = T)
usethis::use_data(nmr_400M,overwrite = T)

data(nmr_hmdb)
data(nmr_400M)
