#load libaries

library(xgboost)
library(caTools)
library(dplyr)
#library(cvms)
library(caret)
library(ggExtra)
library(ROCR)

#read data
data <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")



#We are considering borderline diabetic patients as diabetic
data <- data %>%
  mutate(diab = case_when(data$Diabetes == 0 ~ 0, data$Diabetes == 1 ~ 1, data$Diabetes == 2 ~ 1))


selected_columns <- c("HeartDiseaseorAttack", "Age", "Stroke", "GenHlth", "Sex", "BMI", "PhysHlth", "DiffWalk", "MentHlth", "diab", "Education", "Income")

df <- data[selected_columns]

set.seed(123)
train_index <- sample(nrow(df), 0.80 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]


glm_model <- glm(HeartDiseaseorAttack ~ ., data = train_data, family = "binomial")
saveRDS(glm_model, file = "glm_model.rds")

predictions <- predict(glm_model, newdata = test_data, type = "response")


predictions_factor <- factor(ifelse(predictions >= 0.5, 1, 0), levels = c(1, 0))

test_data$HeartDiseaseorAttack <- factor(test_data$HeartDiseaseorAttack, levels = c(1, 0))

conf_matrix_glm <- confusionMatrix(predictions_factor, test_data$HeartDiseaseorAttack)

print(conf_matrix_glm)

roc_curve_glm <- roc(test_data$HeartDiseaseorAttack, predictions)
plot(roc_curve_glm, main = "ROC Curve", col = "blue", print.auc = TRUE, lwd = 4)

auc(roc_curve_glm)


binary_visualiseR(
  train_labels = predictions_factor,
  truth_labels = test_data$HeartDiseaseorAttack,
  class_label1 = "Heart Disease",
  class_label2 = "No Heart Disease",
)
