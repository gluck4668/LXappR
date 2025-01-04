library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

# devtools::load_all()

pic_dir="D:/Desktop/LXRapp_shiny 2025-1-4/dataset/hmdb_400 pic"

hmdb_id_name <- "D:/Desktop/LXRapp_shiny 2025-1-4/dataset/nmr/Chenomx_hmdb_id.xlsx"

OCRpic(pic_dir,hmdb_id_name)


