
R_packs <- function(){
  installed_packs <- installed.packages()[,1]
  packs <-c("tidyverse","caret","keras","tensorflow","shiny","openxlsx","stringr","dplyr","reticulate")
  install.packages(c(packs[!packs %in% installed_packs]))
  sapply(packs, function(i) library(i,character.only = TRUE))
}

R_packs()

# 1. 数据准备（假设数据格式：期号,r1,r2,r3,r4,r5,r6,blue）
# 数据示例：
# 2023001,1,5,9,12,25,30,7
# 2023002,3,8,11,18,22,33,12
data <- read.xlsx("dataset/lottery_data02.xlsx") %>%
         mutate(across(everything(), as.numeric)) # 批量转换所有字符列为数字列
colnames(data) <- c("date",paste0("r",1:6),"blue")

data <- data %>%
  mutate(across(r1:blue, as.integer)) %>%
  filter(complete.cases(.))  # 移除缺失值

# write.csv(data,"dataset/lottery_df.csv",row.names = FALSE)

# 增强验证步骤
validate_numbers <- function(red, blue) {
  stopifnot(
    all(red >= 1 & red <= 33),
    all(blue >= 1 & blue <= 16),
    apply(red, 1, function(x) length(unique(x)) == 6)  # 确保无重复红球
  )
}

# 红球处理（排序+去重）
red_balls <- data[, paste0("r", 1:6)] %>%
  apply(1, sort) %>%
  t() %>%
  as.matrix()

# 蓝球处理
blue_balls <- data$blue

# 执行验证
validate_numbers(red_balls, blue_balls)


# 稳健编码方案 ----------------------------------------------------------
# 红球编码（多标签二进制矩阵）
create_red_matrix <- function(numbers) {
  matrix <- array(0, dim = c(nrow(numbers), 33))
  for (i in 1:nrow(numbers)) {
    matrix[i, numbers[i, ]] <- 1
  }
  return(matrix)
}

# 蓝球编码（单类独热编码）
create_blue_matrix <- function(numbers) {
  matrix <- array(0, dim = c(length(numbers), 16))
  for (i in seq_along(numbers)) {
    matrix[i, numbers[i]] <- 1
  }
  return(matrix)
}

# 生成编码矩阵
red_binary <- create_red_matrix(red_balls)
blue_binary <- create_blue_matrix(blue_balls)

# 时间序列重构 ----------------------------------------------------------
build_sequences <- function(red, blue, n_steps = 5) {
  n_samples <- nrow(red) - n_steps
  X <- array(dim = c(n_samples, n_steps, 33 + 16))
  y_red <- array(dim = c(n_samples, 33))
  y_blue <- array(dim = c(n_samples, 16))

  for (i in (n_steps+1):nrow(red)) {
    X[i-n_steps,,] <- cbind(
      red[(i-n_steps):(i-1), ],
      blue[(i-n_steps):(i-1), ]
    )
    y_red[i-n_steps,] <- red[i, ]
    y_blue[i-n_steps,] <- blue[i, ]
  }
  return(list(X = X, y_red = y_red, y_blue = y_blue))
}

# 生成时间序列数据
sequences <- build_sequences(red_binary, blue_binary)
X <- sequences$X
y_red <- sequences$y_red
y_blue <- sequences$y_blue

# 模型架构重构 ----------------------------------------------------------
build_model <- function(input_shape) {
  inputs <- layer_input(shape = input_shape)

  # 特征提取层
  lstm_out <- inputs %>%
    layer_lstm(128, return_sequences = TRUE) %>%
    layer_layer_normalization() %>%
    layer_dropout(0.3) %>%
    layer_lstm(64) %>%
    layer_layer_normalization() %>%
    layer_dropout(0.2)

  # 红球输出（多标签分类）
  red_output <- lstm_out %>%
    layer_dense(128, activation = "relu") %>%
    layer_dense(33, activation = "sigmoid", name = "red_output")

  # 蓝球输出（单标签分类）
  blue_output <- lstm_out %>%
    layer_dense(64, activation = "relu") %>%
    layer_dense(16, activation = "softmax", name = "blue_output")

  model <- keras_model(
    inputs = inputs,
    outputs = list(red_output, blue_output)
  )

  return(model)
}

# 编译模型
model <- build_model(c(dim(X)[2], dim(X)[3]))

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = list(
    red_output = "binary_crossentropy",
    blue_output = "sparse_categorical_crossentropy"  # 修改损失函数
  ),
  metrics = list(
    red_output = "accuracy",
    blue_output = "accuracy"
  )
)

# 模型训练 ------------------------------------------------------------
history <- model %>% fit(
  x = X,
  y = list(y_red, y_blue),
  epochs = 100,
  batch_size = 64,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(patience = 10),
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5)
  )
)

# 预测模块 ------------------------------------------------------------
predict_next <- function(model, last_n_games) {
  # 输入形状应为 (1, n_steps, 49)
  pred <- model %>% predict(last_n_games)

  # 红球解码（概率前6的非重复球）
  red_probs <- pred[[1]][1, ]
  red_numbers <- head(order(red_probs, decreasing = TRUE), 6) %>% sort()

  # 蓝球解码（概率最高值）
  blue_number <- which.max(pred[[2]][1, ])

  return(list(red = red_numbers, blue = blue_number))
}

# 执行预测
latest_sequence <- array_reshape(
  tail(X, 1),
  dim = c(1, dim(X)[2], dim(X)[3])
)

prediction <- predict_next(model, latest_sequence)
cat("预测红球:", prediction$red, "\n预测蓝球:", prediction$blue)


