
install.packages("devtools")
library(devtools)
install_github("gluck4668/LXimpala")

library(LXimpala)

#--------------------------------
data(meta_data_example)
data(gene_meta_example)
data(protein_meta_example)
#--------------------------------

rm(list=ls())

devtools::load_all()

data_file <- "dataset/impala/meta_ORA_results.csv" # 数据文件，格式csv
data_type <- "metabolites"  # 数据类型："metabolite", "gene-metabolite","protein-metabolites"

LXimpala(data_file,data_type)
