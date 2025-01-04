
library(dplyr)
library(openxlsx)
library(stringr)

file_dir <- dir("nmr_identificatin_2024-12-26",full.names = TRUE)

d_list <- list()

for (file in file_dir){
  d <- read.xlsx(file)
  f_name <- str_extract(file, "(?<=\\/).*?(?=_)")
  d_list[[f_name]] <- d$name

}


# 初始化交集为第一个向量
common_chars <- d_list[[1]]

# 逐个向量求交集
for (vec in d_list[-1]) {
  common_chars <- intersect(common_chars, vec)
}

# 打印结果
print(common_chars)
