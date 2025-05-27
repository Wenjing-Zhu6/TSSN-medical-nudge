# 加载必要的包
library(readxl)
library(grf)
library(ggplot2)
library(dplyr)
library(openxlsx)

# 1. 读取数据
file_path <- "/Users/zhu/Desktop/smart nudge/smart nudge-analysis/processed_11.29_3 days.xlsx"
sheet_name <- "1"
# 读取 Excel 数据
df <- read_excel(file_path, sheet = sheet_name)

# 2. 因果森林的变量设置
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

# 设置随机种子，确保结果可重复
set.seed(42)  

# 3. 初始化 K-fold 交叉验证
k_folds <- 10  # 10折交叉验证
mse_values <- numeric(k_folds) # 初始化一个向量来存储每个 fold 的 MSE

# 手动实现交叉验证
for (i in 1:k_folds) {
  # 创建训练集和测试集的索引
  set.seed(i)  # 每次迭代设置不同的随机种子，以确保每次划分不同
  train_index <- sample(1:nrow(X), size = 0.8 * nrow(X))  # 随机选取80%的数据作为训练集
  X_train <- X[train_index,]
  y_train <- y[train_index]
  T_train <- T[train_index]
  
  X_test <- X[-train_index,]
  y_test <- y[-train_index]
  T_test <- T[-train_index]
  
  # 训练因果森林模型
  model <- causal_forest(X_train, T_train, y_train)
  
  # 预测 CATE
  cate_estimates <- predict(model, X_test)$predictions
  
  # 计算当前 fold 的 MSE
  mse <- mean((cate_estimates - y_test)^2)  # 计算均方误差
  mse_values[i] <- mse  # 保存 MSE
}

# 计算并打印平均 MSE
average_mse <- mean(mse_values)
cat("平均 MSE: ", average_mse, "\n")

# 4. 训练最终模型，并计算 CATE 结果
final_model <- causal_forest(X, T, y)
args(causal_forest)  # 输出模型的参数
cate_estimates <- predict(final_model, X)$predictions

# 5. 保存数据
output_file <- "/Users/zhu/Desktop/cate.xlsx" # 创建文件路径
wb <- createWorkbook() #创建一个新的工作簿

# 第一个工作表：原始数据 + CATE
df$CATE <- cate_estimates  
addWorksheet(wb, "Data with CATE")
writeData(wb, "Data with CATE", df) 

# 第二个工作表：单独存放 CATE 和 y
cate_results <- data.frame(true = y, predicted_cate = cate_estimates)
addWorksheet(wb, "CATE and Y")
writeData(wb, "CATE and Y", cate_results) 

# 第三个工作表：特征重要性
importance <- variable_importance(final_model)
# 将特征重要性整理为数据框
importance_df <- data.frame(
  Feature = colnames(X),
  Importance = round(importance, 3)
) %>% arrange(desc(Importance))
# 打印特征重要性
print("Feature Importances:")
print(importance_df)
# 将特征重要性保存到一个新的 sheet
addWorksheet(wb, "Feature Importance")
writeData(wb, "Feature Importance", importance_df)

# 保存工作簿到文件
saveWorkbook(wb, output_file, overwrite = TRUE)

# 6. 可视化
# CATE 分布图
ggplot(cate_results, aes(x = predicted_cate)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "#00adb7", alpha = 0.7) +  
  geom_density(color = "red", linewidth = 1) +  # 替换 size 为 linewidth
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),        # 移除所有网格线
    axis.ticks = element_blank(),        # 移除坐标轴刻度线
    axis.text = element_blank(),         # 移除坐标轴刻度文字
    axis.title = element_blank()         # 移除坐标轴标题（其实上面labs已经处理过）
  )
  #labs(title = "Distribution of CATE Estimates", x = "CATE Estimates", y = "Density") +
  #theme_minimal()
 
#ggsave("/Users/zhu/Desktop/cate_distribution_plot.png")

# 特征重要性图
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importances from Causal Forest", x = "Feature", y = "Importance") +
  theme_minimal()
#ggsave("/Users/zhu/Desktop/feature_importance_plot.png")
