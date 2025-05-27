# 加载必要的包
library(readxl)
library(grf)
library(dplyr)
library(ggplot2)

# 1. 读取数据
file_path <- "/Users/zhu/Desktop/smart nudge/smart nudge-analysis/processed_11.29_3 days.xlsx"
sheet_name <- "1"

df <- read_excel(file_path, sheet = sheet_name)

# 选择需要的特征变量
X <- df %>%
  select(gender, address, insurance_type, starts_with("dept"),
         starts_with("age_") & !starts_with("age_category"),
         starts_with("amount_category_"))

T <- df$info_type  # 处理变量
T[T == 0] <- 0     
T[T == 1] <- 1   
y <- df$if_paid    # 结果变量

# 确保 X 是数值型矩阵
X <- as.matrix(X)
T <- as.numeric(T)
y <- as.numeric(y)

# 2. 初始化因果森林模型
set.seed(42)
model <- causal_forest(X, T, y)

# 3. 预测 CATE
df$cate <- predict(model)$predictions

# 4. 分组分析（按 `age_` 变量）
age_groups <- c("age_0-25", "age_26-45", "age_46-60", "age_61-90")

# 保存分组结果
results <- list()

for (group in age_groups) {
  group_data <- df %>% filter(.data[[group]] == 1)
  
  mean_cate <- mean(group_data$cate, na.rm = TRUE)
  se_cate <- sd(group_data$cate, na.rm = TRUE) / sqrt(nrow(group_data))
  
  # 保存结果
  results[[group]] <- list(
    mean_cate = mean_cate,
    se_cate = se_cate,
    n = nrow(group_data)
  )
}

# 5. 显著性检验（T 检验：两两分组比较）
pairwise_results <- combn(age_groups, 2, simplify = FALSE)

# 创建 DataFrame 存储 T 检验结果
t_test_results <- data.frame(
  Group1 = character(),
  Group2 = character(),
  Mean_CATE_Diff = numeric(),
  p_value = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

for (pair in pairwise_results) {
  group1 <- df %>% filter(.data[[pair[1]]] == 1) %>% pull(cate)
  group2 <- df %>% filter(.data[[pair[2]]] == 1) %>% pull(cate)
  
  t_test <- t.test(group1, group2, var.equal = FALSE)  # Welch's t-test（方差不齐）
  
  mean_diff <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
  
  # 存入 DataFrame
  t_test_results <- rbind(t_test_results, data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    Mean_CATE_Diff = mean_diff,
    p_value = t_test$p.value,
    CI_Lower = t_test$conf.int[1],
    CI_Upper = t_test$conf.int[2]
  ))
}

# 6. 将 T 检验结果保存到桌面
output_path <- "~/Desktop/t_test_results.csv"  # 文件路径
write.csv(t_test_results, output_path, row.names = FALSE)

cat(sprintf("\nT 检验结果已保存至: %s\n", output_path))

# 7. 可视化分组 CATE 结果
group_means <- sapply(results, function(res) res$mean_cate)
group_se <- sapply(results, function(res) res$se_cate)

group_df <- data.frame(
  Group = age_groups,
  Mean_CATE = group_means,
  SE = group_se
)

ggplot(group_df, aes(x = Group, y = Mean_CATE)) +
  geom_bar(stat = "identity", fill = "#15455f", alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean_CATE - SE, ymax = Mean_CATE + SE), width = 0.2) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# 可选：保存可视化结果
# ggsave("~/Desktop/age_group_cate_plot.png")
