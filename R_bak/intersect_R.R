# 定义字符向量
vec1 <- c("a", "b", "c", "d", "e", "f")
vec2 <- c("a", "r", "b", "d")
vec3 <- c("q", "a", "n", "d")
vec4 <- c("a", "s", "d", "p")
vec5 <- c("l", "a", "b", "e", "d", "g", "h", "o", "u", "y")

# 将所有向量放入一个列表中
vectors <- list(vec1, vec2, vec3, vec4, vec5)

# 初始化交集为第一个向量
common_chars <- vectors[[1]]

# 逐个向量求交集
for (vec in vectors[-1]) {
  common_chars <- intersect(common_chars, vec)
}

# 打印结果
print(common_chars)
