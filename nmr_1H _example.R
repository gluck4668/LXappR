
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

devtools::load_all()

test_file <- "dataset/nmr/test.xlsx"

lib_file <- "dataset/nmr/nmr_400M.rds"

# lib_file <- NA

se=0.1


nmr_1H (test_file=test_file,lib_file=lib_file,se=se)

