
# -------1. 加载必要的 R 包--------
#install.packages("tidyverse")  # 数据处理
#install.packages("caret")      # 数据分割
#install.packages("keras")      # 深度学习框架
#install.packages("tensorflow") # TensorFlow 后端
#install.packages("shiny")      # 创建交互式应用

R_packs <- function(){
installed_packs <- installed.packages()[,1]
packs <-c("tidyverse","caret","keras","tensorflow","shiny","openxlsx","stringr","dplyr","reticulate")
install.packages(c(packs[!packs %in% installed_packs]))
sapply(packs, function(i) library(i,character.only = TRUE))
}

R_packs()

# https://pypi.org/project/
py <- py_config()
py
use_python(py[[1]])
# use_python("C:/Users/gluck/Documents/.virtualenvs/r-tensorflow/Scripts/python.exe")

#在配置完成后，运行以下代码验证 TensorFlow 是否正确安装：
tf$constant("Hello, TensorFlow!")
# tf.Tensor(b'Hello, TensorFlow!', shape=(), dtype=string)说明 TensorFlow 安装成功

#----- 2. 加载数据------
# 建立文件夹
lott_dir <- c("lotter result")
if(!dir.exists(lott_dir))
  dir.create(lott_dir)

data <- read.xlsx(data_file)

#----- 3. 数据预处理-----
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

#------4. 划分训练集和测试集------
#set.seed(42)  # 设置随机种子以确保可重复性
train_index <- createDataPartition(1:nrow(X), p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train_red <- y_red[train_index, ]
y_test_red <- y_red[-train_index, ]
y_train_blue <- y_blue[train_index, ]
y_test_blue <- y_blue[-train_index, ]

#------5. 构建深度学习模型------
# 使用 Keras 构建一个多层感知器（MLP）模型来预测红球和蓝球号码。
# 构建红球模型
red_model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 6, activation = "softmax")  # 输出 6 个红球

red_model <- keras_model_sequential(name = "red_model") %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(ncol(X_train))) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 6, activation = "softmax")  # 输出 6 个红球

# 编译模型
red_model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

# 将目标变量转换为分类格式
y_train_red_cat <- to_categorical(y_train_red - 1, num_classes = 33)
y_test_red_cat <- to_categorical(y_test_red - 1, num_classes = 33)

# 训练模型
red_model %>% fit(
  as.matrix(X_train),
  y_train_red_cat,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)












