
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

devtools::load_all()

test_file <- "dataset/nmr/test.xlsx"

# lib_file <- c("dataset/nmr/nmr_400M.rds","dataset/nmr/lib_nmr_hmdb.rds")

lib_file <- c("hmdb","400M")

lib_file <- c("D:/Desktop/LXappR 2024-11-22/dataset/nmr/lib.xlsx",
              "D:/Desktop/LXappR 2024-11-22/dataset/nmr/nmr_400M.rds")

lib_select <- "standard"

lib_select <- "other"

# lib_file <- NA

se=0.1


nmr_1H (test_file=test_file,lib_file=lib_file,lib_select,se=se)

