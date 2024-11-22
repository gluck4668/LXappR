
install.packages("devtools")
library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

devtools::load_all()

data_file <- "dataset/impala/meta_ORA_results.csv" # 数据文件，格式csv
data_type <- "gene-metabolite"  # 数据类型："metabolite", "gene-metabolite","protein-metabolites"

impala(data_file,data_type)
