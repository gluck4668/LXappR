
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

devtools::load_all()

test_file <- "D:/Desktop/LXappR 2024-11-01/dataset/nmr/test.xlsx"

# lib_file <- "D:/Desktop/LXappR 2024-11-01/dataset/nmr/lib.xlsx"

lib_file <- NA

se=0.1


nmr_1H (test_file=test_file,lib_file=lib_file,se=se)
