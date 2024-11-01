library(openxlsx)

lib_nmr_hmdb <- read.xlsx("lib_nmr_hmdb.xlsx")

usethis::use_data(lib_nmr_hmdb,overwrite = T)

data(lib_nmr_hmdb)
