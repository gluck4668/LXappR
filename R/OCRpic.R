
#在R语言中，批量识别图片上的数字可以使用OCR（光学字符识别）技术

OCRpic <- function(pic_dir,hmdb_id_name){

# 加载R包
R_pack <- function(){
packs <- c("tesseract","dplyr","openxlsx","stringr")
not.installed <- !packs %in% installed.packages()[,1]

if(any(not.installed))
  install.packages(c(packs[not.installed]))

sapply(packs,function(x) library(x,character.only = TRUE))
}

R_pack()

#rm(list=ls())

image_dir <- paste0("OCRpic results ",Sys.Date())
if(!dir.exists(image_dir))
  dir.create(image_dir)

# 设置tesseract可执行文件的路径（如果需要）
# 例如：tesseract_path <- "C:/Program Files/Tesseract-OCR/tesseract.exe"
# Sys.setenv(TESSERACT_PATH = tesseract_path)

# 指定包含图片的目录
image_directory <- pic_dir

# 获取目录中所有图片文件的文件名
image_files <- list.files(path = image_directory, pattern = "\\.(png|jpg|jpeg|bmp|tiff)$", full.names = TRUE)

# 创建一个空列表来存储识别结果
data_list <- list()

# 遍历每个图片文件并进行OCR识别
x=1
for (x in 1:length(image_files)) {
  cat("Processing:", x," of ", length(image_files), "images", "\n")

  # 使用tesseract进行OCR识别
  text <- tesseract::ocr(image_files[x])

  # 使用strsplit函数按换行符分割字符串
  number_list <- strsplit(text, "\n")[[1]] %>% as.numeric()

  # 存储结果
  compound_name <- str_extract(image_files[x],"(?<=/)[^/]*(?=\\.)")

  data_list[[compound_name]] <- number_list
}

#----将列表转换为数据框-----------
# 找到最长行的长度
max_length <- max(sapply(data_list, length))

# 创建一个数据框，用NA填充缺失值
df <- data.frame(
  name = rep(NA, max_length),  # 初始化name列，稍后将覆盖
  shift = rep(NA, max_length)    # 初始化一个占位列
)

# 填充数据框
i=1
for (i in length(data_list)) {
  # 填充name
  df$name[1:length(data_list[[i]])] <- names(data_list)[i]  # 仅填充有数据的部分

  # 填充值到新的列中（为了避免覆盖，我们需要动态创建列名或直接处理）
  # 这里为了简化，我们使用一个矩阵来转置填充，然后合并
  df <- t(sapply(data_list, function(x) c(x, rep(NA, max_length - length(x)))))
  colnames(df) <- paste0("shift", 1:max_length)

}

df <- data.frame(df)

df$name <- rownames(df)
df <- df[,c(ncol(df),1:(ncol(df)-1))]
rownames(df) <- c(1:nrow(df))

id_name <- read.xlsx(hmdb_id_name)

df_save <- inner_join(id_name,df,by="name")

file_save <- paste0(image_dir,"/",str_extract(pic_dir,"(?<=\\/).*"),".xlsx")

write.xlsx(df_save, file_save)

print(paste0("The result can be found in the folder of '",  image_dir,"'"  ))
}





