library(devtools)
install_github("gluck4668/LXappR")

library(LXappR)

rm(list=ls())

# devtools::load_all()

# 截图所在的文件夹
pic_dir="D:/Desktop/New packs/LXappR 2025-1-6/dataset/hmdb_400 pic"

# 指定hmdb_name_id文件
hmdb_id_name <- "D:/Desktop/New packs/LXappR 2025-1-6/dataset/nmr/Chenomx_hmdb_name_id_list.xlsx"

#如果没有name-id列表，则填NULL
# hmdb_id_name <- NULL

OCRpic(pic_dir,hmdb_id_name)


