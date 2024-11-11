
nmr_1H <- function(test_file=test_file,lib_file=lib_file,se=se){

  library(dplyr)
  library(openxlsx)
  library(stringr)
  library(data.table)
  library(foreach)

#---数据基本信息------------------
dir_save= paste0("nmr_identificatin_",Sys.Date())
if(!dir.exists(dir_save))
  dir.create(dir_save)

# 参考库
if(!is.na(lib_file))
{file_ext <- tools::file_ext(lib_file)
if(grepl("rds",file_ext,ignore.case = T))
  lib_nmr_hmdb <- readRDS(lib_file) else
    lib_nmr_hmdb <- eval(str2expression(paste0("read.",file_ext,"('",lib_file,"')")))
} else
    data("lib_nmr_hmdb")

lib <- lib_nmr_hmdb
# rm(lib_nmr_hmdb)

# 检测结果
test_result <- read.xlsx(test_file)

x=1

foreach(x=c(1:nrow(test_result)),.errorhandling="pass") %do% {

  print( paste0("Number ",x, " of ", nrow(test_result), " samples is being processed...")  )
  message ( paste0("Number ",x, " of ", nrow(test_result), " samples is being processed...")  )

  test_id <- test_result[x,] %>% as.character() %>% .[1]
  test_x <- test_result[x,] %>% as.character() %>% .[-1] %>% as.numeric()
  test_x <- test_x[!is.na(test_x)]

#-----参考库里的代谢物与检测结果进行比对-------
lib_y_tab <- data.frame()

y=1

foreach(y=c(1:nrow(lib)),.errorhandling="pass") %do% {

  print( paste0("Number ",y, " of ", nrow(lib), " metabolite libraries was processed...")  )

#---每个代谢物的所有化学位移
lib_y <- lib[y,] %>% .[,-c(1:2)] %>% as.numeric() %>% .[!is.na(.)]

lib_yy_tem <- "NA"
lib_test_tem <- "NA"

yy=1

foreach(yy=c(1:length(lib_y)),.errorhandling="pass") %do% {

#---每次一个代谢物的一个化学位移,逐个进行比对
lib_yy <- lib_y[yy]

z=1

foreach(z=c(1:length(test_x)),.errorhandling="pass") %do% {

  l_test <- test_x[z]-se
  h_test <- test_x[z]+se

if(l_test<=lib_yy & lib_yy<=h_test)
  lib_test_tem <- paste0(lib_test_tem,",",lib_yy,"(",test_x[z],")") else
    lib_test_tem <- paste0(lib_test_tem,",",lib_yy,"(FALSE)")

  } # foreach z end

#-----去掉NA，并去重--------
lib_test_tem <- unlist(strsplit(lib_test_tem, ","))[-1] %>% .[!duplicated(.)]

#-----去掉FASLE-----
if(length(lib_test_tem)>1)
  lib_test_tem <- lib_test_tem[!grepl("FALSE",lib_test_tem)]

#----如果有多个临近值，要整合一下-----
if(length(lib_test_tem)>1){
  lib_test_tem <- paste0(lib_test_tem,collapse = ",")
  lib_test_tem <- gsub(paste0(",",lib_yy),"/", lib_test_tem)
  }

#----跳出foreach z 循环,把结果上传到上一级的 test_tem ，即进入foreach y -------
lib_yy_tem <- paste0(lib_yy_tem,",",lib_test_tem)
#lib_yy_tem <- unlist(strsplit(lib_yy_tem, ","))[-1]

} # foreach yy end

#----跳出foreach yy 循环,把结果上传到上一级 -------
lib_y_text <- lib[y,c(1,2)] %>% as.character()
lib_yy_tab <- c(lib_y_text,unlist(strsplit(lib_yy_tem, ","))[-1]) %>% as.data.frame() %>% t() %>% as.data.frame()

if(y==1)
lib_y_tab <- rbind(lib_y_tab,lib_yy_tab) else
{
  if (ncol(lib_yy_tab) == ncol(lib_y_tab)){
    lib_y_tab <- rbind(lib_y_tab,lib_yy_tab)
    } else{

      if(ncol(lib_yy_tab)<ncol(lib_y_tab)){
        n <- ncol(lib_y_tab)-ncol(lib_yy_tab)
        s1 <-ncol(lib_yy_tab)+1
        s2 <- ncol(lib_yy_tab)+n
        lib_yy_tab[,c(s1:s2)] <- NA} else
        {n <- ncol(lib_yy_tab)-ncol(lib_y_tab)
        s1 <-ncol(lib_y_tab)+1
        s2 <- ncol(lib_y_tab)+n
        lib_y_tab[,c(s1:s2)] <- NA}
      lib_y_tab <- rbind(lib_y_tab,lib_yy_tab)

    }

}

} # foreach y end

df <- lib_y_tab

rownames(df) <- c(1:nrow(df))
nn <- ncol(df)-2
colnames(df) <- c("id","name", paste0("shift",c(1:nn))  )
df$identified <- NA

for(i in 1:nrow(df)){
  if( any(grepl("FALSE",df[i,])) )
    df[i,ncol(df)] <- FALSE else
      df[i,ncol(df)] <- TRUE
}

file_all_save <- paste0(dir_save,"/",test_id,"_sample_all.xlsx")
# write.xlsx(df,file_all_save)

# 删除所有值都是NA的列
df_sub <- subset(df,identified==TRUE)
df_sub <- df_sub[, colSums(is.na(df_sub)) < nrow(df_sub)]

file_identified <- paste0(dir_save,"/",test_id,"_identified.xlsx")
if(nrow(df_sub)>0)
   write.xlsx(df_sub,file_identified)

#--只显示检测数据---
test_fun <- function (x){
  x %>% str_extract(.,"(?=\\().*") %>%
        str_remove_all(.,"[()]") }

df_test <- df_sub

n_sub <- ncol(df_test)-1

i=1
for(i in 1:nrow(df_test)){
df_test[i,c(2:n_sub)] <- test_fun(df_test[i,c(2:n_sub)])}

message ( paste0("Number ",x, " of ", nrow(test_result), " samples was done.")  )

} # foreach x end


print(paste0("The identification result can be found in the folder of '",dir_save,"'"))

}

