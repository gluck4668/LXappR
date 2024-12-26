

d1 <- data.frame(id=c(1:3),name=c("a","b","c"),val=c(1:3))
d2 <- data.frame(id=c(1:5),name=c("a","b","c","d","e"),val=c(1:5))
d3 <- data.frame(id=c(5:9),name=c("a","b","e","r","g"),val=c(5:9))
# R语言，如何以name来取三个表的并集

# 加载dplyr包
library(dplyr)

# 定义数据框
d1 <- data.frame(id=c(1:3), name=c("a","b","c"), val=c(1:3))
d2 <- data.frame(id=c(1:5), name=c("a","b","c","d","e"), val=c(1:5))
d3 <- data.frame(id=c(5:9), name=c("a","b","e","r","g"), val=c(5:9))

# 使用full_join合并数据框
result <- d1 %>%
  full_join(d2, by = "name") %>%
  full_join(d3, by = "name")

# 打印结果
print(result)


# 逐个向量求交集
d_list <- list(d1,d2,d3)
d_com <- d_list[[1]]
for (vec in d_list[-1]) {
  d_com <- rbind(d_com, vec)
}

d_com <- distinct(d_com,name,.keep_all = T)
