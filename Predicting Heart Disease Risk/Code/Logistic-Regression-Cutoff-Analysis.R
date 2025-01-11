library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(reshape2)
library(pROC)
library(car)
library(ggplot2)
library(ConfusionTableR)


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

logistic <- glm(HeartDiseaseorAttack ~ Age+Stroke+GenHlth+Sex+BMI+DiffWalk+MentHlth+diab+Income+CholCheck+NoDocbcCost, data = train_set, family = "binomial")
summary(logistic)
probs <- predict(logistic, newdata = test_set, type = "response")

Accuracy <- 0
Sensitivity <- 0
Specificity <- 0
#Threshold <- seq(0, 1, length.out = 100)

for (c in 1:100){
  pred <- ifelse(probs > c/100, 1, 0)
  cm = confusionMatrix(factor(pred), factor(test_set$HeartDiseaseorAttack), positive = as.character(1))
  Threshold[c] <- c/100
  Accuracy[c] <- cm$overall['Accuracy']
  Sensitivity[c] <- cm$byClass['Sensitivity']
  Specificity[c] <- cm$byClass['Specificity']
} 

df <- data.frame(Threshold,Accuracy, Sensitivity, Specificity)

  
dd_tidyr <- pivot_longer(df, cols = -Threshold, names_to = "Temperature")

ggplot(dd_tidyr) +
       geom_line(aes(x = Threshold, y = value, colour = Temperature)) +
       scale_colour_manual(values = c("red", "green", "blue"))

