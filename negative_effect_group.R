# 加载必要的包
library(readxl)
library(grf)
library(dplyr)
library(tidyr)
library(writexl)

# 1. 读取数据
file_path <- "/Users/zhu/Desktop/smart nudge/smart nudge-analysis/processed_11.29_3 days.xlsx"
sheet_name <- "1"
df <- read_excel(file_path, sheet = sheet_name)

# 因果森林的变量设置
X <- df %>%
  select(gender, address, insurance_type, starts_with("dept"), 
         starts_with("age_") & !starts_with("age_category"), 
         starts_with("amount_category_"))

T <- df$info_type  # 处理变量
T[T == 0] <- 0     
T[T == 1] <- 1     
y <- df$if_paid    # 结果变量
X <- as.matrix(X)
T <- as.numeric(T)
y <- as.numeric(y)

# 初始化因果森林模型
model <- causal_forest(X, T, y)

# 预测 CATE 和标准误差（se计算使用因果森林内置的方法，逻辑是bootstrap）
set.seed(42)  # 设置固定随机种子
predicted_cate <- predict(model)$predictions
cate_se <- predict(model, estimate.variance = TRUE)$variance

# 将预测结果和标准误差添加到数据框
df$predicted_cate <- predicted_cate 
df$cate_se <- cate_se  

# 定义组合（两个变量）
#variables <- c("age", "gender", "department", "address", "insurance_type", "amount")
variables <- c("age", "gender", "amount")
combinations <- combn(variables, 2, simplify = FALSE)

# 定义特殊分组逻辑
special_grouping <- function(df, variable) {
  case_when(
    variable == "department" ~ case_when(
      df$dept_1 == 1 ~ "Dept_1",
      df$dept_2 == 1 ~ "Dept_2",
      df$dept_3 == 1 ~ "Dept_3",
      df$dept_4 == 1 ~ "Dept_4",
      TRUE ~ "Unknown"
    ),
    variable == "age" ~ case_when(
      df$`age_0-25` == 1 ~ "0-25",
      df$`age_26-45` == 1 ~ "26-45",
      df$`age_46-60` == 1 ~ "46-60",
      df$`age_61-90` == 1 ~ "61-90",
      TRUE ~ "Unknown"
    ),
    variable == "amount" ~ case_when(
      df$`amount_category_Low(2-61)` == 1 ~ "Low",
      df$`amount_category_Medium(62-135)` == 1 ~ "Medium",
      df$`amount_category_High(>135)` == 1 ~ "High",
      TRUE ~ "Unknown"
    ),
    TRUE ~ as.character(df[[variable]])
  )
}

# 初始化结果存储
all_results <- list()

# 循环分析每个组合
for (combo in combinations) {
  # 动态创建分组变量
  df_grouped <- df %>%
    mutate(group_type = paste(
      special_grouping(., combo[1]),
      special_grouping(., combo[2]),
      sep = " & "
    ))
  
  # 分组分析
  group_analysis <- df_grouped %>%
    group_by(group_type) %>%
    summarise(
      mean_cate = mean(predicted_cate, na.rm = TRUE),
      sd_cate = sd(predicted_cate, na.rm = TRUE),
      count = n()
    )
  
  # 计算标准误差
  group_analysis <- group_analysis %>%
    mutate(se_cate = sqrt(mean(cate_se^2, na.rm = TRUE))) 
  
  # 使用每个分组的标准误差计算方法
 # group_analysis <- group_analysis %>%
    #mutate(se_cate = sapply(group_type, function(g) {
      #mean(cate_se[df_grouped$group_type == g], na.rm = TRUE)  # 每组的标准误差为该组内的均值
   # }))
  
  # 计算 t 值和 p 值，基于每组的标准误差
  group_analysis <- group_analysis %>%
    mutate(
      t_value = mean_cate / se_cate,  # 计算 t 值
      p_value = 2 * pt(abs(t_value), df = count - 1, lower.tail = FALSE)  # 计算 p 值 (自由度 = count - 1)
    )
  
  # 计算置信区间
  group_analysis <- group_analysis %>%
    mutate(
      lower_ci = mean_cate - 1.96 * se_cate,  # 计算95%置信区间的下限
      upper_ci = mean_cate + 1.96 * se_cate   # 计算95%置信区间的上限
    )
  
  # 保存当前组合结果
  all_results[[paste(combo, collapse = "_")]] <- group_analysis
}

# 保存所有结果到一个 Sheet
combined_results <- bind_rows(
  lapply(names(all_results), function(name) {
    all_results[[name]] %>%
      mutate(combo = name)  # 增加组合来源列
  })
)

# 将组合来源列放在第一列，方便筛选
combined_results <- combined_results %>%
  relocate(combo, .before = group_type)

# 筛选：p < 0.01 且 mean_cate < 0
filtered_results <- combined_results %>%
  filter(p_value < 0.01 & mean_cate < 0)

# 将所有结果和筛选后的结果保存到 Excel 文件
write_xlsx(list(
  AllResults = combined_results,  # 保存所有结果
  FilteredResults = filtered_results  # 保存筛选后的结果
), "/Users/zhu/Desktop/two_variables_with_p_and_CI.xlsx")


