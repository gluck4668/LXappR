
lottery_forest <- function(data_file) {

#  安装相关R包
installed_packs <- installed.packages()[,1]
packs <-c("tidyverse","caret","randomForest","openxlsx","stringr","dplyr")
install.packages(c(packs[!packs %in% installed_packs]))
sapply(packs, function(i) library(i,character.only = TRUE))

# rm(list=ls())

# 建立文件夹
lott_dir <- c("lotter result")
if(!dir.exists(lott_dir))
  dir.create(lott_dir)


ext <- tools::file_ext(data_file)
if(tolower(ext)=="xlsx")
   data <- read.xlsx(data_file) else
     data <- read.csv(data_file)

colnames(data) <- c("Date",paste0("Red",1:6),"Blue")

# 数据预处理
red_balls <- data %>% select(Red1, Red2, Red3, Red4, Red5, Red6)
red_balls <- red_balls %>% mutate(across(everything(), as.numeric)) # 批量转换所有字符列为数字列

blue_ball <- data %>% select(Blue)
blue_ball <- blue_ball %>% mutate(across(everything(), as.numeric)) # 批量转换所有字符列为数字列

# 添加红球和蓝球的历史出现频率作为特征
for (num in 1:33) {
  red_balls[[paste0("Red", num, "_freq")]] <- rowSums(red_balls == num)
}

for (num in 1:16) {
  blue_ball[[paste0("Blue", num, "_freq")]] <- rowSums(blue_ball == num)
}

# 合并特征
X <- bind_cols(red_balls, blue_ball)
y_red <- red_balls  # 目标是预测红球
y_blue <- blue_ball  # 目标是预测蓝球

# 划分训练集和测试集
# set.seed(42)
train_index <- createDataPartition(1:nrow(X), p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train_red <- y_red[train_index, ]
y_test_red <- y_red[-train_index, ]
y_train_blue <- y_blue[train_index, ]
y_test_blue <- y_blue[-train_index, ]

# 训练红球模型
red_models <- list()
for (i in 1:6) {
  red_models[[i]] <- randomForest(x = X_train, y = y_train_red[[i]], ntree = 100)
}

# 训练蓝球模型
blue_model <- randomForest(x = X_train, y = y_train_blue$Blue, ntree = 100)

# 预测下一期号码

# new_data

# 1. 使用历史数据的平均值
new_data_1 <- data.frame(
  Red1 = mean(red_balls$Red1),
  Red2 = mean(red_balls$Red2),
  Red3 = mean(red_balls$Red3),
  Red4 = mean(red_balls$Red4),
  Red5 = mean(red_balls$Red5),
  Red6 = mean(red_balls$Red6),
  Blue = mean(blue_ball$Blue)
)

# 2. 使用历史数据的众数
new_data_2 <- data.frame(
  Red1 = as.numeric(names(sort(table(red_balls$Red1), decreasing = TRUE)[1])),
  Red2 = as.numeric(names(sort(table(red_balls$Red2), decreasing = TRUE)[1])),
  Red3 = as.numeric(names(sort(table(red_balls$Red3), decreasing = TRUE)[1])),
  Red4 = as.numeric(names(sort(table(red_balls$Red4), decreasing = TRUE)[1])),
  Red5 = as.numeric(names(sort(table(red_balls$Red5), decreasing = TRUE)[1])),
  Red6 = as.numeric(names(sort(table(red_balls$Red6), decreasing = TRUE)[1])),
  Blue = as.numeric(names(sort(table(blue_ball$Blue), decreasing = TRUE)[1]))
)


# 3. 基于模型的预测逻辑

new_data_3 <- tail(X, 1)  # 使用最近一期的特征值

# 4. 随机生成合理的值
# set.seed(42)  # 设置随机种子以确保可重复性
new_data_4 <- data.frame(
  Red1 = sample(1:33, 1),
  Red2 = sample(1:33, 1),
  Red3 = sample(1:33, 1),
  Red4 = sample(1:33, 1),
  Red5 = sample(1:33, 1),
  Red6 = sample(1:33, 1),
  Blue = sample(1:16, 1)
)

new_data_5 <- data.frame(
  Red1 = sample(1:33, 1),
  Red2 = sample(1:33, 1),
  Red3 = sample(1:33, 1),
  Red4 = sample(1:33, 1),
  Red5 = sample(1:33, 1),
  Red6 = sample(1:33, 1),
  Blue = sample(1:16, 1)
)


new_list <- list(new_df1=new_data_1,
                 new_df2=new_data_2,
                 new_df3=new_data_3,
                 new_df4=new_data_4,
                 new_df5=new_data_5
                 )


predic_table_all <- data.frame()

i=1

for(i in 1:5) {

new_data <-new_list[[i]]

# 添加频率特征
for (num in 1:33) {
  new_data[[paste0("Red", num, "_freq")]] <- sum(new_data[1, 1:6] == num)
}

for (num in 1:16) {
  new_data[[paste0("Blue", num, "_freq")]] <- sum(new_data[1, 7] == num)
}

# 预测 6 个红球
predicted_red <- sapply(red_models, function(model) predict(model, new_data))
predicted_red <- sort(unique(predicted_red))  # 去重并排序
if (length(predicted_red) < 6) {
  # 如果预测结果不足 6 个，随机补充
  remaining_balls <- setdiff(1:33, predicted_red)
  predicted_red <- c(predicted_red, sample(remaining_balls, 6 - length(predicted_red)))
}

predicted_red <- sort(predicted_red)  %>% round() # 取整数

# 预测 1 个蓝球
predicted_blue <- predict(blue_model, new_data) %>% round() # 取整数

# 输出预测结果
#print(paste("预测的红球号码是:", paste(predicted_red, collapse = ", ")))
#print(paste("预测的蓝球号码是:", predicted_blue))

predic_table <- data.frame(red=predicted_red) %>% t() %>% data.frame()
colnames(predic_table) <- paste0("Red",1:ncol(predic_table))
rownames(predic_table) <- paste0("mehtod",i)
predic_table$blue <- as.numeric(predicted_blue)

predic_table_all <- rbind(predic_table_all,predic_table)

}

write.xlsx(predic_table_all,paste0(lott_dir,"/predic_forest_",Sys.Date(),".xlsx"))

print(predic_table_all)

}

#----------------------------------------------------------------------------------------


lottery_xgboost <- function(data_file) {

  #  安装相关R包
  installed_packs <- installed.packages()[,1]
  packs <-c("tidyverse","caret","xgboost","Matrix","openxlsx","stringr","dplyr")
  install.packages(c(packs[!packs %in% installed_packs]))
  sapply(packs, function(i) library(i,character.only = TRUE))

  # rm(list=ls())

  # 建立文件夹
  lott_dir <- c("lotter result")
  if(!dir.exists(lott_dir))
    dir.create(lott_dir)

  ext <- tools::file_ext(data_file)
  if(tolower(ext)=="xlsx")
    data <- read.xlsx(data_file) else
      data <- read.csv(data_file)

  colnames(data) <- c("Date",paste0("Red",1:6),"Blue")

  # 数据预处理
  red_balls <- data %>% select(Red1, Red2, Red3, Red4, Red5, Red6)
  red_balls <- red_balls %>% mutate(across(everything(), as.numeric)) # 批量转换所有字符列为数字列

  blue_ball <- data %>% select(Blue)
  blue_ball <- blue_ball %>% mutate(across(everything(), as.numeric)) # 批量转换所有字符列为数字列


  # 添加红球和蓝球的历史出现频率作为特征
  for (num in 1:33) {
    red_balls[[paste0("Red", num, "_freq")]] <- rowSums(red_balls == num)
  }

  for (num in 1:16) {
    blue_ball[[paste0("Blue", num, "_freq")]] <- rowSums(blue_ball == num)
  }

  # 合并特征
  X <- bind_cols(red_balls, blue_ball)
  y_red <- red_balls  # 目标是预测红球
  y_blue <- blue_ball  # 目标是预测蓝球

  # 划分训练集和测试集
  #set.seed(42)
  train_index <- createDataPartition(1:nrow(X), p = 0.8, list = FALSE)
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train_red <- y_red[train_index, ]
  y_test_red <- y_red[-train_index, ]
  y_train_blue <- y_blue[train_index, ]
  y_test_blue <- y_blue[-train_index, ]

  # 定义 XGBoost 参数
  params_red <- list(
    objective = "reg:squarederror",  # 回归任务
    max_depth = 6,                  # 树的最大深度
    eta = 0.1,                      # 学习率
    subsample = 0.8,                # 每次使用的样本比例
    colsample_bytree = 0.8          # 每次使用的特征比例
  )

  params_blue <- list(
    objective = "reg:squarederror",  # 回归任务
    max_depth = 6,                  # 树的最大深度
    eta = 0.1,                      # 学习率
    subsample = 0.8,                # 每次使用的样本比例
    colsample_bytree = 0.8          # 每次使用的特征比例
  )

  # 训练红球模型
  dtrain_red <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_red$Red1)
  red_model <- xgb.train(params = params_red, data = dtrain_red, nrounds = 100)

  # 训练蓝球模型
  dtrain_blue <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_blue$Blue)
  blue_model <- xgb.train(params = params_blue, data = dtrain_blue, nrounds = 100)

  # 新数据
  # 预测下一期号码

  # new_data

  # 1. 使用历史数据的平均值
  new_data_1 <- data.frame(
    Red1 = mean(red_balls$Red1),
    Red2 = mean(red_balls$Red2),
    Red3 = mean(red_balls$Red3),
    Red4 = mean(red_balls$Red4),
    Red5 = mean(red_balls$Red5),
    Red6 = mean(red_balls$Red6),
    Blue = mean(blue_ball$Blue)
  )

  # 2. 使用历史数据的众数
  new_data_2 <- data.frame(
    Red1 = as.numeric(names(sort(table(red_balls$Red1), decreasing = TRUE)[1])),
    Red2 = as.numeric(names(sort(table(red_balls$Red2), decreasing = TRUE)[1])),
    Red3 = as.numeric(names(sort(table(red_balls$Red3), decreasing = TRUE)[1])),
    Red4 = as.numeric(names(sort(table(red_balls$Red4), decreasing = TRUE)[1])),
    Red5 = as.numeric(names(sort(table(red_balls$Red5), decreasing = TRUE)[1])),
    Red6 = as.numeric(names(sort(table(red_balls$Red6), decreasing = TRUE)[1])),
    Blue = as.numeric(names(sort(table(blue_ball$Blue), decreasing = TRUE)[1]))
  )


  # 3. 基于模型的预测逻辑

  new_data_3 <- tail(X, 1)  # 使用最近一期的特征值

  # 4. 随机生成合理的值
  # set.seed(42)  # 设置随机种子以确保可重复性
  new_data_4 <- data.frame(
    Red1 = sample(1:33, 1),
    Red2 = sample(1:33, 1),
    Red3 = sample(1:33, 1),
    Red4 = sample(1:33, 1),
    Red5 = sample(1:33, 1),
    Red6 = sample(1:33, 1),
    Blue = sample(1:16, 1)
  )

  new_data_5 <- data.frame(
    Red1 = sample(1:33, 1),
    Red2 = sample(1:33, 1),
    Red3 = sample(1:33, 1),
    Red4 = sample(1:33, 1),
    Red5 = sample(1:33, 1),
    Red6 = sample(1:33, 1),
    Blue = sample(1:16, 1)
  )


  new_list <- list(new_df1=new_data_1,
                   new_df2=new_data_2,
                   new_df3=new_data_3,
                   new_df4=new_data_4,
                   new_df5=new_data_5
  )


  predic_table_all <- data.frame()

  i=1

  for(i in 1:5) {

    new_data <-new_list[[i]]


    # 添加频率特征（确保与训练数据一致）
    for (num in 1:33) {
      new_data[[paste0("Red", num, "_freq")]] <- sum(new_data[1, 1:6] == num)
    }

    for (num in 1:16) {
      new_data[[paste0("Blue", num, "_freq")]] <- sum(new_data[1, 7] == num)
    }

    # 确保 new_data 的特征名称与 X_train 一致
    colnames(new_data) <- colnames(X_train)

    # 预测 6 个红球
    dnew_red <- xgb.DMatrix(data = as.matrix(new_data))
    predicted_red <- predict(red_model, dnew_red)
    predicted_red <- sort(unique(round(predicted_red)))  # 去重并排序
    if (length(predicted_red) < 6) {
      # 如果预测结果不足 6 个，随机补充
      remaining_balls <- setdiff(1:33, predicted_red)
      predicted_red <- c(predicted_red, sample(remaining_balls, 6 - length(predicted_red)))
    }
    predicted_red <- sort(predicted_red)  # 最终排序

    # 预测 1 个蓝球
    dnew_blue <- xgb.DMatrix(data = as.matrix(new_data))
    predicted_blue <- round(predict(blue_model, dnew_blue))

    # 输出预测结果
   # print(paste("预测的红球号码是:", paste(predicted_red, collapse = ", ")))
   # print(paste("预测的蓝球号码是:", predicted_blue))

    predic_table <- data.frame(red=predicted_red) %>% t() %>% data.frame()
    colnames(predic_table) <- paste0("Red",1:ncol(predic_table))
    rownames(predic_table) <- paste0("mehtod",i)
    predic_table$blue <- as.numeric(predicted_blue)

    predic_table_all <- rbind(predic_table_all,predic_table)

  }

  write.xlsx(predic_table_all,paste0(lott_dir,"/predic_xgboost_",Sys.Date(),".xlsx"))

  print(predic_table_all)


}
