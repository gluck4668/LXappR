library(openxlsx)

lib_nmr_hmdb <- read.csv ("lib_nmr_hmdb.csv")

usethis::use_data(lib_nmr_hmdb,overwrite = T)

data(lib_nmr_hmdb)
