# 加载必要的包
library(readxl)
library(grf)
library(ggplot2)
library(dplyr)

# 1. 读取数据
file_path <- "/Users/zhu/Desktop/smart nudge/smart nudge-analysis/processed_11.29_7 days.xlsx"
sheet_name <- "1"

# 读取 Excel 数据
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

# 确保 X 是数值型矩阵
X <- as.matrix(X)
T <- as.numeric(T)
y <- as.numeric(y)

# 2. 初始化因果森林模型
set.seed(42)  # 设置固定随机种子
model <- causal_forest(X, T, y)

# 3. 预测 CATE
df$predicted_cate <- predict(model)$predictions

# 4. 分组分析：按 `gender` 分组
group_analysis <- df %>%
  group_by(gender) %>%
  summarise(
    mean_cate = mean(predicted_cate, na.rm = TRUE),
    sd_cate = sd(predicted_cate, na.rm = TRUE),
    count = n(),
    se_cate = sd_cate / sqrt(count)  # 计算标准误 SE
  )

# 输出分组分析结果
print(group_analysis)

# 5. 差异性显著性分析
group_0 <- df %>% filter(gender == 0) %>% pull(predicted_cate)
group_1 <- df %>% filter(gender == 1) %>% pull(predicted_cate)

t_test_result <- t.test(group_0, group_1, var.equal = FALSE)  # Welch's T-test（方差不齐）

# 计算均值差异
mean_diff <- mean(group_0, na.rm = TRUE) - mean(group_1, na.rm = TRUE)

# 存入 DataFrame
t_test_results <- data.frame(
  Group1 = "Female",
  Group2 = "Male",
  Mean_CATE_Diff = mean_diff,
  p_value = t_test_result$p.value,
  CI_Lower = t_test_result$conf.int[1],
  CI_Upper = t_test_result$conf.int[2]
)

# 6. 将 T 检验结果保存到桌面
output_path <- "~/Desktop/t_test_results.csv"
write.csv(t_test_results, output_path, row.names = FALSE)

cat(sprintf("\nT 检验结果已保存至: %s\n", output_path))

# 7. 可视化分组分析结果
ggplot(group_analysis, aes(x = factor(gender), y = mean_cate)) +
  geom_bar(stat = "identity", fill = "#15455f", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_cate - se_cate, ymax = mean_cate + se_cate), 
                width = 0.2) +
  labs(x = NULL, y = NULL) +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +  # 设置分组标签
  theme_minimal()

# 可选：保存可视化结果
# ggsave("~/Desktop/gender_group_cate_plot_with_significance.png")
