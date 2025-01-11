library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(reshape2)
library(pROC)
#library(car)
library(ggplot2)
library(ConfusionTableR)


# plot_location <- 'Other Resources/Plots/'
data <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")
data$HeartDiseaseorAttack <- as.factor(data$HeartDiseaseorAttack)

#Is data cleaning required? (check for missing values in the entire dataframe)
any(is.na(data))    #False, so no missing values in data

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)


# Summary statistics
summary(data)

#From summary, we observe that most of the population has not experienced a heart attack
#most people had their cholesterol checked in the last 5 years,
#the mean BMI is 28.38. most consume vegetables atleast once a day. most are not heavy drinkers
#median/mean age group is about 8, corresponding to 55-59 years old
#median income is $50k-75k, mean income is $35k-50k.

#Since diabetes and heart disease outcomes are strongly correlated,
#What's the mean of people that have/had diabetes in population?
#0 = no diabetes, 1 = prediabetes/borderline diabetes, 2 = has diabetes
#Maps the value 2 to 1, indicating the person has had diabetes during pregnancy or has diabetes

unique(data$Diabetes)


data <- data %>%
  mutate(diab = case_when(data$Diabetes == 0 ~ 0, data$Diabetes == 1 ~ 1, data$Diabetes == 2 ~ 1))

#Calculate the mean for diabetes. mean ~ 0.158. 15.8% of population has prediabetes/borderline/diabetes
sum(data['diab'])/nrow(data)

#remove diab column for analysis
data = subset(data, select=-c(diab))


data0 <- subset(data, HeartDiseaseorAttack == 0)
data1 <- subset(data, HeartDiseaseorAttack == 1)

plot_no_risk <- ggplot(data0, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "No Heart Attack/Risk",
       x = "Age",
       y = "Count") +
  theme_minimal()

plot_risk<- ggplot(data1, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
  labs(title = "Heart Attack/Risk",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Combine plots
library(gridExtra)
grid.arrange(plot_no_risk, plot_risk, ncol = 2)


# correlation matrix
correlation_matrix <- cor(data)
# print(correlation_matrix)

# correlation matrix
ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()




#Is there multicollinearity among any variables? For the sake of our analysis,
#we are determining that VIF > 5 as collinearity

#We suspect age, high blood pressure, high cholesterol, smoking, diabetes, 
#physical activity, and heart disease risk may have a correlation or collinearity

age_VIF_model <- lm(Age ~ . - HeartDiseaseorAttack, data = data)
vif(age_VIF_model)   #No collinearity observed

bp_VIF_model <- lm(HighBP ~ . - HeartDiseaseorAttack, data = data)
vif(bp_VIF_model)   #No collinearity observed

#repeat VIF analysis for other variables (regress variable over other predictors). 
#We found no collinearity spotted among any predictor variables


# Set seed for reproducibility
set.seed(123)

train_index <- sample(nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Train the logistic regression model
model <- glm(HeartDiseaseorAttack ~ ., data = train_data, family = "binomial")

# Summary of the model
summary(model)


# Which features are most statistically significant?
# p values of significance [0, 0.001] include the intercept, HighBP, HighChol, CholCheck,
#Smoker, Stroke, Diabetes, HvyAlcoholConsump, NoDocbcCost, GenHlth, DiffWalk, Sex, Age,
#and Income

#Note: it is odd to see the coefficient for heavy alcohol consumption 
#(HvyAlcoholConsump) to be negative since we would expect the opposite. 
#This would suggest that being a heavy alcohol consumer (men having > 14 drinks per week, 
#adult women > 7 drinks per week) results in a lower probability of heart attack. 
#This may differ once we incorporate other metrics into our model 
#for example, interaction


# Make predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")


# Convert predictions to a factor with levels 0 and 1
predictions_factor <- factor(ifelse(predictions > 0.5, 1, 0), levels = c(1, 0))

# Convert test_data$HeartDiseaseorAttack to a factor with levels 0 and 1
test_data$HeartDiseaseorAttack <- factor(test_data$HeartDiseaseorAttack, levels = c(1, 0))


# Compute confusion matrix
conf_matrix <- confusionMatrix(predictions_factor, test_data$HeartDiseaseorAttack)
print(conf_matrix$table)

# Compute AUC-ROC
roc_curve <- roc(test_data$HeartDiseaseorAttack, predictions)
auc_score <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue")


# Extract performance metrics
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- conf_matrix$byClass["F1"]

# Print performance metrics
cat("Performance Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("AUC-ROC:", auc_score, "\n")
cat("Confusion Matrix:\n")
print(conf_matrix$table)



binary_visualiseR(
  train_labels = predictions_factor,
  truth_labels = test_data$HeartDiseaseorAttack,
  class_label1 = "Heart Disease",
  class_label2 = "No Heart Disease",
)





