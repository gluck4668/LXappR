
# install.packages("xml2")

# 加载R包
library(xml2)
library(stringr)
library(dplyr)
library(openxlsx)

rm(list=ls())

file.copy("HMDB_Reference_Cards/.","lib/.",overwrite = T,recursive = T)

xmc <- dir("lib",full.names = T)
for(x in xmc){
  new_name <- gsub(".xmc",".xml",x)
  file.rename(x, new_name)
  }

xml_file <- dir("lib",full.names = T)
hmdb_df <- data.frame()

y=1

for(y in 1:length(xml_file)) {

print(paste0("Number ",y," of ",length(xml_file)," is processed..."))

# 读取XML文件
xml_data <- read_xml(xml_file[y])

# 提取metaCompound节点
# 如果命名空间复杂，通常需要手动检查结构或使用其他工具确认路径
meta_compound <- xml_find_first(xml_data, "//*[local-name()='metaCompound']")

# 检查是否找到节点
if (is.na(meta_compound)) {
  cat("No metaCompound node found.\n")
} else {
  # 提取common名称
  compound_name <- xml_text(xml_find_first(meta_compound, ".//*[local-name()='names']/*[local-name()='common']"))
  }

# 提取hmdb id
xx <- as.character(xml_data)
hmdb_id <- str_extract(xx,"(?<=id=\"HMDB).*(?=\" type)")
hmdb_id <- paste0("HMDB00",hmdb_id)

df <- data.frame(id=hmdb_id,name=compound_name)
hmdb_df <- rbind(hmdb_df,df)

}

hmdb_df <- hmdb_df %>% arrange(name)

hmdb_df <- distinct(hmdb_df,name,.keep_all = T)

table(duplicated(hmdb_df$id))
table(duplicated(hmdb_df$name))

write.xlsx(hmdb_df,"hmdb_400M.xlsx")


#-----------------
c400 <- readRDS("dataset/nmr/nmr_400M.rds") %>% .[,c(1:2)]
c400$lib <- "c400"
c500 <- readRDS("dataset/nmr/nmr_500M.rds")  %>% .[,c(1:2)]
c500$lib <- "c500"
h400 <- read.xlsx("dataset/nmr/hmdb_400M.xlsx")
h400$lib <- "h400"

dd <- rbind(c400,c500) %>% rbind(.,h400) %>%
  distinct(.,id,.keep_all = T) %>%
  distinct(.,name,.keep_all = T)

table(duplicated(dd$id))
table(duplicated(dd$name))





