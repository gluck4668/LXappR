
ttest <- function(data_file){

#------安装R包------
R_packs <- function(){

all_packs <- installed.packages()[,1]

packs <- c("openxlsx","dplyr","nortest","car","curl","openssl","stringr")

bioc_packs <- c("limma")

install.packages(packs[!packs %in% all_packs])

if(!bioc_packs %in% all_packs)
BiocManager::install(bioc_packs[!bioc_packs %in% all_packs])

sapply(c(packs,bioc_packs), function(x) library(x, character.only = T))

}

R_packs()

#------建立文件夹------
dir_save <- paste0("ttest_results ",Sys.Date())

if(!dir.exists(dir_save))
  dir.create(dir_save)

f_ext <- tools::file_ext(data_file) # tools::file_ext查看文件扩展名

data <- eval(str2expression(paste0("read.",f_ext,"(data_file)"))) %>% na.omit()

#---id去重并取平均值---
table(duplicated(data[,1]))
data <- limma::avereps(data[,-1],data[,1]) %>% data.frame() # 对重复的项，取其平均值，同时也有去重功能
df <- sapply(data,function(x) as.numeric(x)) %>% data.frame()# 转为数字格式
rownames(df) <- rownames(data)

#---查看组别-----
col_names <- colnames(df)
group_names <- gsub("\\d+$","",col_names) %>% unique() #去掉字符串后面的数字

group1 <- grep(group_names[1],col_names) %>% as.numeric()
group2 <- grep(group_names[2],col_names) %>% as.numeric()

#---计算FC和log2FC----
df$FC <- rowMeans(df[,c(group2)])/rowMeans(df[,c(group1)])
df$FC <- round(df$FC,4)
df$log2FC <- log2(df$FC) %>% round(.,4)

#---计算pvale----
df$pvale <- NA
for(i in 1:nrow(df)){
  t_test <- t.test(df[i,c(group2)],df[i,c(group1)])
  df[i,ncol(df)] <- t_test[["p.value"]] %>% round(.,4)
  }

df$p.adjust <- p.adjust(df$pvale,method = "BH") %>% round(.,4)

file_save <- paste0(dir_save,"/","t.test_rsult (",str_extract(data_file, "(?<=/)[^/]*(?=\\.)"),").xlsx")

write.xlsx(df,file_save,rowNames=T)

print( paste0("The t test result can be found in the fold of '", dir_save,"'"))

}











