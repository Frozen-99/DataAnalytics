library(caTools)
library(dplyr)
library(caret)
library(ROCR)
library(corrplot)
library(tidyr)
library(reshape2)
library(pROC)
library(ggplot2)
library(ConfusionTableR)

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

# split data into train and test sets
set.seed(123)
train_index <- sample(nrow(df), 0.80 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# train logistic regression model
glm_model <- glm(HeartDiseaseorAttack ~ ., data = train_data, family = "binomial")
saveRDS(glm_model, file = "Models/glm-base-model.rds")

# predictions
predictions <- predict(glm_model, newdata = test_data, type = "response")

predictions_factor <- factor(ifelse(predictions >= 0.5, 1, 0), levels = c(1, 0))
test_data$HeartDiseaseorAttack <- factor(test_data$HeartDiseaseorAttack, levels = c(1, 0))


# confusion matrix
conf_matrix <- confusionMatrix(predictions_factor, test_data$HeartDiseaseorAttack)

cat("Confusion Matrix:\n")
print(conf_matrix)


# roc curve 
roc_curve <- roc(test_data$HeartDiseaseorAttack, predictions)
plot(roc_curve, lwd = 4, main = "Logistic Regression ROC Curve", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.05, paste("AUC - ", round(auc(roc_curve), 3)), adj = c(0.5, -0.5), cex = 1.2, col = "blue")


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
cat("AUC:", auc(roc_curve), "\n")


# plot confusion matrix
conf_matrix_table <- conf_matrix$table
rownames(conf_matrix_table) <- c("Heart Disease", "No Heart Disease")
colnames(conf_matrix_table) <- c("Heart Disease", "No Heart Disease")

conf_df <- as.data.frame.table(conf_matrix_table)
names(conf_df) <- c("Predicted", "Actual", "Count")

conf_df$Percentage <- round(conf_df$Count / sum(conf_df$Count) * 100, 2)


ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count, label = paste0(Count, "\n(", Percentage, "%)"))) + #paste0(Percentage, "% (", Count, ")"))) +
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




