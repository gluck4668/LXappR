library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

# devtools::load_all()

data_file <- "D:/Desktop/New packs/LXappR 2025-1-10/dataset/SAR_ttest 2025-01-10.xlsx"

ttest(data_file)


