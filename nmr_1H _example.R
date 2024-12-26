
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

devtools::load_all()

# test_file <- "dataset/nmr/test.xlsx"

test_file <- "dataset/nmr/SAR.xlsx"

lib_file <- c("400M","500M")

# lib_file <- c("dataset/nmr/lib.xlsx","dataset/nmr/nmr_400M.rds")

lib_select <- "standard"

# lib_select <- "other"

se=0.1

nmr_1H (test_file=test_file,lib_file=lib_file,lib_select,se=se)
