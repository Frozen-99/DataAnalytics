library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(reshape2)
library(pROC)
library(car)
library(ggplot2)
library(ConfusionTableR)


# data$HeartDiseaseorAttack <- as.factor(data$HeartDiseaseorAttack)

#paste(names(data), collapse = '+')

pr = "HighBP+HighChol+CholCheck+BMI+Smoker+Stroke+Diabetes+PhysActivity+Fruits+Veggies\
+HvyAlcoholConsump+AnyHealthcare+NoDocbcCost+GenHlth+MentHlth+PhysHlth+DiffWalk+Sex+Age+Education+Income"

data <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")

data = data[data$BMI < 80,]




data <- data %>%
   mutate(diab = case_when(data$Diabetes == 0 ~ 0, data$Diabetes == 1 ~ 1, data$Diabetes == 2 ~ 1))


# Set seed for reproducibility
set.seed(123)

train_index <- sample(nrow(data), 0.80 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Train the logistic regression model
model <- glm(HeartDiseaseorAttack ~ HighBP+HighChol+CholCheck+BMI+Smoker+Income+GenHlth+MentHlth+PhysHlth
             +HvyAlcoholConsump+ Stroke*HighChol*Sex*GenHlth
             +Stroke+PhysActivity+Diabetes+Sex+Age, data = train_data,family = "binomial")

AIC(model)

#model <- glm(HeartDiseaseorAttack ~ HighBP+HighChol+CholCheck+BMI+Smoker+Income+GenHlth+MentHlth+PhysHlth
#          +HvyAlcoholConsump+ Stroke*HighChol*Sex*GenHlth
#             +Stroke+PhysActivity+diab+Sex+Age, data = data, family = "binomial")

#summary(model)
# Make predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")


# Convert predictions to a factor with levels 0 and 1
predictions_factor <- factor(ifelse(predictions > 0.25, 1, 0), levels = c(1, 0))

# Convert test_data$HeartDiseaseorAttack to a factor with levels 0 and 1
test_data$HeartDiseaseorAttack <- factor(test_data$HeartDiseaseorAttack, levels = c(1, 0))


# Compute confusion matrix
conf_matrix <- confusionMatrix(predictions_factor, test_data$HeartDiseaseorAttack)
print(conf_matrix)
print(conf_matrix$table)



