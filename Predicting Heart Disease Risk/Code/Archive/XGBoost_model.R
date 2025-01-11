#load libaries

library(xgboost)
library(caTools)
library(dplyr)
library(cvms)
library(caret)
library(ggExtra)
library(ROCR)

#read data
df <- read.csv("heart_disease_health_indicators_BRFSS2015.csv")

#clean BMI data
df = df[df$BMI < 80,]

#We are considering borderline diabetic patients as diabetic
df <- df %>%
  mutate(diab = case_when(df$Diabetes == 0 ~ 0, df$Diabetes == 1 ~ 1, df$Diabetes == 2 ~ 1))

#Delete Diabetes variable since we created diab
df <- subset(df, select=-Diabetes)

#Set seed for reproducibility and split data into training and test sets 80:20
set.seed(123)
sample_split <- sample.split(Y = df$HeartDiseaseorAttack, SplitRatio = 0.8)
train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)

y_train <- as.integer(train_set$HeartDiseaseorAttack)
y_test <- as.integer(test_set$HeartDiseaseorAttack)
X_train <- train_set %>% select(-HeartDiseaseorAttack)
X_test <- test_set %>% select(-HeartDiseaseorAttack)

#Construct DMatrix data structure for XGBoost model
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "binary:logistic",
  eval_metric = "auc"
)

#Build model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100, #further optimizations can experiment with nrounds. nrounds corresponds to how many decision trees to compute.
  verbose = 1
)

xgb_model

#Feature Importance to determine important features
importance_matrix <- xgb.importance(
    feature_names = colnames(xgb_train),
    model = xgb_model
)
importance_matrix

xgb.plot.importance(importance_matrix)

#Make predictions with XGBoost model. Convert to dataframe. Produces prediction probability of heart disease for every individual
xgb_preds <- predict(xgb_model, newdata = as.matrix(X_test),  type = 'response') 
xgb_preds <- data.frame(XGBoost_probability = xgb_preds)


#Add columns of the predicted result for each individual
xgb_preds <- xgb_preds %>%
    mutate(PredictedClass = ifelse(XGBoost_probability >= 0.5, 1, 0)) 

xgb_preds$ActualClass <- y_test
xgb_preds

#Model Evaluation
conf_matrix <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- conf_matrix$byClass["F1"]

#ROC AUC
pred <- prediction(xgb_preds$XGBoost_probability, y_test)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf, colorize=T)
auc.perf <- performance(pred, measure='auc')
auc.perf@y.values
