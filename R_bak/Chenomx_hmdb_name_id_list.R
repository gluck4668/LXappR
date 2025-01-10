
# install.packages("xml2")

# 加载R包
library(xml2)
library(stringr)
library(dplyr)
library(openxlsx)

rm(list=ls())

# 设置文件夹路径
folder_path <- "HMDB_Reference_Cards"

# 获取所有 .xmc 文件的完整路径列表
xmc_files <- list.files(
  path = folder_path,      # 指定文件夹路径
  pattern = "\\.xmc$",     # 正则表达式，匹配 .xmc 文件
  full.names = TRUE  )      # 返回完整路径

hmdb_df <- data.frame()

y=1

for(y in 1:length(xmc_files)) {

print(paste0("Number ",y," of ",length(xmc_files)," is processed..."))

# 读取XML文件
xmc_data <- read_xml(xmc_files[y])

# 提取metaCompound节点
# 如果命名空间复杂，通常需要手动检查结构或使用其他工具确认路径
info <- xml_find_first(xmc_data, "//*[local-name()='metaCompound']")

# 检查是否找到节点
if (is.na(info)) {
  cat("No metaCompound node found.\n")
} else {
  # 提取common名称
  hmdb_name <- xml_text(xml_find_first(info, ".//*[local-name()='names']/*[local-name()='common']"))
  }

# 提取hmdb id
info_char <- as.character(info)
hmdb_id <- str_extract(info_char,"(?<=id=\"HMDB).*(?=\" type)")
hmdb_id <- paste0("HMDB00",hmdb_id)

df <- data.frame(id=hmdb_id,name=hmdb_name)
hmdb_df <- rbind(hmdb_df,df)

}

hmdb_df <- hmdb_df %>% arrange(name)

hmdb_df <- distinct(hmdb_df,name,.keep_all = T)

write.xlsx(hmdb_df,"Chenomx_hmdb_name_id_list.xlsx")








