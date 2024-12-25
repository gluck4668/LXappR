
nmr_1H <- function(test_file=test_file,lib_file=lib_file,lib_select,se=se){

#------安装R包--------------------------------------
R_pack <- function(){
  installed_packs <- installed.packages()[,1]
  packs <- c("openxlsx","dplyr","stringr","data.table","foreach")
  not_packs <- packs[!packs %in% installed_packs]

  if(length(not_packs)>0){
    packs_fun <- function(i){install.packages(i)}
    sapply(not_packs,packs_fun)
  }

  lib_fun <- function(i){library(i,character.only = T)}
  sapply(packs,lib_fun)
}

R_pack()

#---数据基本信息------------------
dir_save= paste0("nmr_identificatin_",Sys.Date())
if(!dir.exists(dir_save))
  dir.create(dir_save)

# 参考库
lib_list <- list()
i=1
if(lib_select =="standard"){
  if(length(lib_file)>0){
    for(i in 1:length(lib_file)){
      lib_name <- paste0("nmr_",lib_file[i])
      eval(str2expression(paste0("data(",lib_name,")")))
      lib_list[[lib_name]] <-  eval(str2expression(lib_name))}
  } else
    stop("Please select at least one standard library.")
  } else {
     if(length(lib_file)>0){
      i=1
       for(i in 1:length(lib_file)){
        lib_name <- str_extract(lib_file[i], "[^/]*(?=\\.)")
        file_ext <- tools::file_ext(lib_file[i])
        if(grepl("rds",file_ext,ignore.case = T))
          lib_data <- readRDS(lib_file[i]) else
          lib_data <- eval(str2expression(paste0("read.",file_ext,"('",lib_file[i],"')")))
          lib_list[[lib_name]] <- lib_data
        }
      } else
       stop("Please select at least one library file.")
    }

# 读取检测数据
test_ext <- tools::file_ext(test_file)
if(grepl("txt",test_ext,ignore.case = T))
  test <- read.table(test_file) else
    test <- eval(str2expression(paste0("read.",test_ext,"('",test_file,"')")))


# 遍历test中的每一列
t=1

for (t in 1:nrow(test)) {
  test_t <- test[t,-c(1)] %>% as.numeric() %>% na.omit() %>% as.numeric() %>% unique()

  print(paste0("Number ",t, " of ", nrow(test)," tests is processing..."))

  # 遍历参考库df中的每一列
  lib_n=1
  foreach(lib_n=c(1:length(lib_list)),.errorhandling="pass") %do% {
   lib_name <- names(lib_list)
   lib_df <- lib_list[[lib_name[lib_n]]]

   # 初始化结果数据框
   result_df <- data.frame()

    x=1
    for (x in 1:nrow(lib_df)) {
     lib_x <- lib_df[x,-c(1:2)] %>% as.numeric() %>% na.omit() %>% as.numeric()
     # 判断lib_x中的每一个数据是否存在于test_t中
     matches <- sapply(lib_x, function(x) any(round(abs(x - test_t),9) <= round(se,9)))
     # 获取每一个匹配的数据
     if(all(matches)){
       val <- lapply(lib_x, function(x) test_t[round(abs(x - test_t),9) <= round(se,9)]) %>%
            unlist() %>% unique() %>% as.character() %>% paste0(.,collapse = ",")
      #  result_col[[col_name_test]] <- val
      result_x <- data.frame(id=lib_df[x,1],name=lib_df[x,2],shift=val)
      colnames(result_x) <- c("id","name","1H-NMR")
      result_df <- rbind(result_df,result_x)
    } #

  } # x end

  file_save <- paste0(dir_save,'/',test[t,1],'_(',lib_name[lib_n],')_results_',Sys.Date(),".xlsx")
  write.xlsx(result_df,file_save)

} # foreach lib_n end

} # t end

print(paste0("The identification result can be found in the folder of '",dir_save,"'"))

}
#-----------------------------------------------------------------------------


