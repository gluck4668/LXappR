# install.packages("devtools")
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())
devtools::load_all()

data_file <- "dataset/lottery_data.xlsx"

lottery(data_file)

lottery_forest(data_file)
lottery_xgboost(data_file)
