library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(reshape2)
library(pROC)
library(car)
library(ggplot2)
library(ConfusionTableR)
library(Boruta)
library(Amelia)
library(modeest)
library(cowplot)
library(mice)
library(caTools)
#Reading the data
df <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")
#Removing rows with BMI more than 80
df = df[data$BMI < 80,]

#We are considering broderline diabetic person as diabetic
df <- data %>%
  mutate(diab = case_when(data$Diabetes == 0 ~ 0, data$Diabetes == 1 ~ 1, data$Diabetes == 2 ~ 1))

# Set seed for reproducibility and split data into train and test 80:20
set.seed(123)
sample_split <- sample.split(Y = df$HeartDiseaseorAttack, SplitRatio = 0.8)
train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)

# Train the logistic regression model
logistic <- glm(HeartDiseaseorAttack ~ ., data = train_set, family = "binomial")
summary(logistic)

#Calculating predictions and confusion matrix
probs <- predict(logistic, newdata = test_set, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)

confusionMatrix(factor(pred), factor(test_set$HeartDiseaseorAttack), positive = as.character(1))

#top 10 important features
importances <- varImp(logistic)  %>%
  arrange(desc(Overall)) %>%
  top_n(10)

#Using Boruta to find important features
boruta_output <- Boruta(HeartDiseaseorAttack ~ ., data = train_set, doTrace = 0)
boruta_output

#Extracting significant attributes  from the output
rough_fix_mod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(rough_fix_mod)
boruta_signif

importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", c("meanImp", "decision")]
importances[order(-importances$meanImp), ]

#Plot output to visualize feature importance
plot(boruta_output, ces.axis = 0.7, las = 2, xlab = "", main = "Feature importance")



