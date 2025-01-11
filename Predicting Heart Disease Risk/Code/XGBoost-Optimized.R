library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(ROSE)
library(ROCR)

# read data
data <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")

# consider borderline diabetic patients as diabetic
data <- data %>%
  mutate(Diabetes = case_when(Diabetes == 0 ~ 0, Diabetes == 1 ~ 1, Diabetes == 2 ~ 1))

# selecting feature important columns; refer Boruta R file
selected_columns <- c("HeartDiseaseorAttack", "Age", "Stroke", "GenHlth", "Sex", 
                      "BMI",  "DiffWalk", "MentHlth", "Diabetes", 
                      "HighBP", "HighChol", "CholCheck", "Smoker")

df <- data[selected_columns]

# handle class imbalance: oversample the minority class
df_balanced <- ROSE(HeartDiseaseorAttack ~ ., data = df, seed = 123)$data


# plot to show data distribtion of HeartDiseaseorAttack before and after oversampling
df$Dataset <- "Before Oversampling"
df_balanced$Dataset <- "After Oversampling"
combined_df <- rbind(df, df_balanced)
# plot - data distribution before and after oversampling
ggplot(combined_df, aes(x = HeartDiseaseorAttack, fill = Dataset, color = Dataset)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Heart Disease or Attack", y = "Density", title = "Dependent Variable Distribution\n(Before & After Over-sampling)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.85, 0.85)) + xlim(-0.25, 1.25) 



# split data into train and test sets w/ 80:20 ratio
set.seed(123)
sample_split <- sample.split(df_balanced$HeartDiseaseorAttack, SplitRatio = 0.8)
train_set <- subset(df_balanced, sample_split == TRUE)
test_set <- subset(df_balanced, sample_split == FALSE)

# separate the dataset into features and target variable
y_train <- as.integer(train_set$HeartDiseaseorAttack)
y_test <- as.integer(test_set$HeartDiseaseorAttack)
X_train <- train_set %>% select(-HeartDiseaseorAttack)
X_test <- test_set %>% select(-HeartDiseaseorAttack)

# construct DMatrix for XGBoost model
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# XGBoost parameters
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.70,
  colsample_bytree = 1,
  objective = "binary:logistic",
  eval_metric = "auc"
)

# build model
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 100)
xgb.save(xgb_model, "Models/xgb-optimized-model")

# predictions
xgb_preds <- predict(xgb_model, newdata = as.matrix(X_test), type = 'response')
xgb_preds <- data.frame(XGBoost_probability = xgb_preds)

# predicted and actual classes to xgb_preds
xgb_preds$PredictedClass <- ifelse(xgb_preds$XGBoost_probability >= 0.5, 1, 0)
xgb_preds$ActualClass <- y_test

# confusion matrix
conf_matrix <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass), positive = "1")

cat("Confusion Matrix:\n")
print(conf_matrix)


# roc curve 
pred <- prediction(xgb_preds$XGBoost_probability, y_test)
perf <- performance(pred, 'tpr', 'fpr')
auc.perf <- performance(pred, measure = 'auc')
plot(perf, colorize = TRUE)
text(0.5, 0.1, paste("AUC: ", round(auc.perf@y.values[[1]], 3)), adj = c(0.5, 0), col = "black", cex = 1.2)


# performance metrics
accuracy <- conf_matrix$overall["Accuracy"]
specificity <- conf_matrix$byClass["Specificity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
precision <- conf_matrix$byClass["Precision"]
f1_score <- conf_matrix$byClass["F1"]



cat("\nPerformance Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Specificity:", specificity, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Precision:", precision, "\n")
cat("F1-score:", f1_score, "\n")
cat("AUC:", auc.perf@y.values[[1]], "\n")


# plot confusion matrix
conf_matrix_table <- conf_matrix$table
rownames(conf_matrix_table) <- c("Heart Disease", "No Heart Disease")
colnames(conf_matrix_table) <- c("Heart Disease", "No Heart Disease")

conf_df <- as.data.frame.table(conf_matrix_table)
names(conf_df) <- c("Predicted", "Actual", "Count")

conf_df$Percentage <- round(conf_df$Count / sum(conf_df$Count) * 100, 2)


ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count, , label = paste0(Count, "\n(", Percentage, "%)"))) + #paste0(Percentage, "% (", Count, ")"))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted"
       #caption = "1: Heart Disease\n0: No Heart Disease"
  ) + 
  geom_text(size = 3, color = "white", hjust = 0.5, vjust = 0.5, fontface = "bold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


