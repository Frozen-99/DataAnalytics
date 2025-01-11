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

model <- glm(HeartDiseaseorAttack ~ HighBP+HighChol+CholCheck+BMI+Smoker+Income+GenHlth+MentHlth+PhysHlth
          +HvyAlcoholConsump+ Stroke*HighChol*Sex*GenHlth
             +Stroke+PhysActivity+diab+Sex+Age, data = data, family = "binomial")

predictions <- predict(model,  type = "response")
predictions_factor <- factor(ifelse(predictions > 0.20, 1, 0), levels = c(1, 0))
actual_factor <- factor(data$HeartDiseaseorAttack, levels = c(1, 0))
conf_matrix <- confusionMatrix(predictions_factor, actual_factor)
print(conf_matrix)

# Summary of the model
#summary(model)



