# 加载必要的包
library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(tools)

#--------------------- 参数设置 ---------------------#
se <- 0.1  # 设置匹配误差范围

#--------------------- 函数定义 ---------------------#
# 读取检测数据文件
read_detection_data <- function(file) {
  df <- read_xlsx(file, col_names = FALSE)
  list(
    groups = df[[1]],
    values = lapply(1:nrow(df), function(i) {
      vals <- as.numeric(df[i, -1])
      vals[!is.na(vals)]
    })
  )
}

# 读取参照数据库文件
read_reference_data <- function(files) {
  map_dfr(files, ~{
    df <- read_xlsx(.x, col_names = FALSE)
    data.frame(
      id = df[[1]],
      name = df[[2]],
      shifts = lapply(1:nrow(df), function(i) {
        s <- as.numeric(df[i, -(1:2)])
        s[!is.na(s)]
      }),
      library = file_path_sans_ext(basename(.x)),
      stringsAsFactors = FALSE
    )
  })
}

#--------------------- 主程序 ---------------------#
# 选择文件（交互式）
cat("请选择检测数据文件（xlsx）...\n")
detection_file <- file.choose()
cat("请选择参照数据库文件（可多选）...\n")
ref_files <- select.list(list.files(), multiple = TRUE, title = "选择参照文件")
ref_files <- c("D:/Desktop/LXappR 25-1-22/dataset/nmr/Chenomx_400M.xlsx",
               "D:/Desktop/LXappR 25-1-22/dataset/nmr/hmdb_400M.xlsx")

# 读取数据
detection_data <- read_detection_data(detection_file)
ref_data <- read_reference_data(ref_files)

# 创建结果目录
result_dir <- "H-NMR_analysis_result"
id_dir <- file.path(result_dir, "identification")
dir.create(id_dir, recursive = TRUE, showWarnings = FALSE)

#--------------------- 第一步：数据比对 ---------------------#
walk(seq_along(detection_data$groups), function(i) {
  group <- detection_data$groups[i]
  values <- detection_data$values[[i]]
  if (length(values) == 0) return()

  matches <- list()

  # 遍历所有参照条目
  for (j in 1:nrow(ref_data)) {
    ref_shift <- ref_data$shifts[[j]]
    if (length(ref_shift) == 0) next

    # 检查所有shift是否匹配
    matched_vals <- map_dbl(ref_shift, ~{
      diffs <- abs(.x - values)
      if (any(diffs <= se)) values[which.min(diffs)] else NA
    })

    if (all(!is.na(matched_vals))) {
      matches[[length(matches)+1]] <- data.frame(
        id = ref_data$id[j],
        name = ref_data$name[j],
        `1H-NMR` = paste(matched_vals, collapse = ", "),
        library = ref_data$library[j],
        stringsAsFactors = FALSE
      )
    }
  }

  # 保存结果
  if (length(matches) > 0) {
    result <- bind_rows(matches) %>%
      distinct(id, .keep_all = TRUE)

    output_file <- paste0(group, "_", Sys.Date(), ".xlsx")
    write_xlsx(result, file.path(id_dir, output_file))
  }
})

#--------------------- 第二步：筛选共同ID ---------------------#
result_files <- list.files(id_dir, pattern = "\\.xlsx$", full.names = TRUE)
if (length(result_files) > 0) {
  # 读取所有结果文件
  all_results <- map(result_files, ~read_xlsx(.x) %>% select(id, name))

  # 查找共同ID
  common_ids <- reduce(map(all_results, ~.x$id), intersect)

  if (length(common_ids) > 0) {
    # 生成汇总文件
    final_result <- all_results[[1]] %>%
      filter(id %in% common_ids) %>%
      distinct(id, .keep_all = TRUE)

    output_file <- paste0(
      file_path_sans_ext(basename(detection_file)),
      "_identified_result_",
      Sys.Date(),
      ".xlsx"
    )
    write_xlsx(final_result, file.path(result_dir, output_file))

    #--------------------- 第三步：筛选个体结果 ---------------------#
    walk(result_files, function(f) {
      df <- read_xlsx(f) %>% filter(id %in% common_ids)
      output_file <- paste0(
        file_path_sans_ext(basename(f)),
        "_identified_",
        Sys.Date(),
        ".xlsx"
      )
      write_xlsx(df, file.path(result_dir, output_file))
    })
  }
}

cat("分析完成！结果保存在", result_dir, "目录中。\n")
