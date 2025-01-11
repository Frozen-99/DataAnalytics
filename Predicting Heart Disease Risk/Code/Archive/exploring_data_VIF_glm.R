#RStudio was used for data analysis
#First step is to save the csv dataset from github locally on your machine, and then update your working directory accordingly

#load data in workspace
data <- read.csv('heart_disease_health_indicators_BRFSS2015.csv')

#Is data cleaning required? (check for missing values in the entire dataframe)
any(is.na(data))    #False, so no missing values in data

#Gather information of variables
summary(data)
#From summary, we observe that most of the population has not experienced a heart attack
#most people had their cholesterol checked in the last 5 years,
#the mean BMI is 28.38. most consume vegetables atleast once a day. most are not heavy drinkers
#median/mean age group is about 8, corresponding to 55-59 years old
#median income is $50k-75k, mean income is $35k-50k.

#Since diabetes and heart disease outcomes are strongly correlated,
#What's the mean of people that have/had diabetes in population?
#0 = no diabetes, 1 = prediabetes/borderline diabetes, 2 = has diabetes
#Maps the value 2 to 1, indicating person has had diabetes during pregnancy or has diabetes
library(dplyr)
data <- data %>%
+ mutate(diab = case_when(Diabetes == 0 ~ 0, Diabetes == 1 ~ 1, Diabetes == 2 ~ 1))

#Calculate the mean for diabetes. mean ~ 0.158. 15.8% of population has prediabetes/borderline/diabetes
sum(data['diab'])/nrow(data)

#remove diab column for analysis
data = subset(data, select=-c(diab))

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

#run logistic regression over all features
model_allvars <- glm(HeartDiseaseorAttack ~ ., data=data, family='binomial')

#Which features are most statistically significant?
# p values of significance [0, 0.001] include the intercept, HighBP, HighChol, CholCheck,
#Smoker, Stroke, Diabetes, HvyAlcoholConsump, NoDocbcCost, GenHlth, DiffWalk, Sex, Age,
#and Income

#Unexepected Observatoin: The oefficient for heavy alcohol consumption is
#(HvyAlcoholConsump) negative. This would suggest that being a heavy alcohol consumer 
#(men having > 14 drinks per week, adult women > 7 drinks per week) results in a 
#lower probability of heart attack. 

#How accurate is this model? Analyze confusion matrix
#Let's make predictions and assume a cutoff value of 0.5

data <- data %>%
+ mutate(pred_prob_model = predict(model_allvars, newdata = ., type='response')) %>%
+ mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1, 0))

xtabs(~HeartDiseaseorAttack + pred_outcome_model, data = data)
#Another way to visualize confusion matrix
install.packages('ConfusionTableR')
library(confusionTableR)

binary_visualiseR(
    train_labels = as.factor(data$pred_outcome_model),
    truth_labels = as.factor(data$HeartDiseaseorAttack),
    class_label1 = "No Heart Disease",
    class_label2 = "Heart Disease",
)
#From this confusion matrix, we observe 227214 true negatives, 2573 false positives,
#20844 false negatives, and 3049 true positives.
#From matrix, we can calculate and observe a sensitivity of 0.1276, a specificity of 0.989,
#precision of 0.542, and an accuracy of 0.9076

#This implies that our model has too many false negatives since the sensitivity
#is low. This is supported by the fact that the specificity is very high, and the 
#precision is low. Intuitively, we would expect these results since the majority 
#of observations do not have heart disease