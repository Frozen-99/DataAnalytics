# Team-13

## Heart Disease Health Indicators

The Behavioral Risk Factor Surveillance System (BRFSS) is an annual health survey conducted by the CDC via telephone, gathering information on health-related risk behaviors, chronic conditions, and the utilization of preventive services from over 400,000 Americans each year. This survey has been consistently administered since 1984. The CSV file of the dataset used for this study is for the year 2015 from Kaggle. The dataset contains responses from 253,680 individuals and encompasses 22 variables.

## Problem Statement

Analyze heart disease data, determine which models best represent the data, and determine the factors that are most relevant in predicting risk of heart disease for patients.

## Primary Research Question (RQ)

- What are the significant risk factors associated with heart disease, and to what extent do they contribute to the occurrence of heart disease in a given population?

- Supporting Research Questions:
	- To what extent can the survey responses from BRFSS be used to predict heart disease?
	- Can a subset of questions from the BRFSS be used to screen for heart disease?

## Justification: 

Heart disease is one of the leading causes of healthcare expenditure in the United States and the leading cause of death. Analysis that provides model analysis and feature importance can be used for a variety of business applications. For example, insurance companies can use screenings to more accurately determine the risk of heart disease and adjust prices of policies accordingly to minimize financial risk with high-risk heart disease patients. The goal is to provide insightful data that can be used for business applications including (but not limited to) advertising and business model optimizations. 

## Dataset/Plan for Data

### Data Sources  

The modified dataset used for this project can be found at the following URL: [https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset](https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset)  

Variable information/explanations corresponding to the modified dataset can be found at the following URL:  [https://www.kaggle.com/code/alexteboul/heart-disease-health-indicators-dataset-notebook/notebook](https://www.kaggle.com/code/alexteboul/heart-disease-health-indicators-dataset-notebook/notebook)  

The original dataset in which the modified dataset was built from can be found at the following URL:  [https://www.kaggle.com/datasets/cdc/behavioral-risk-factor-surveillance-system](https://www.kaggle.com/datasets/cdc/behavioral-risk-factor-surveillance-system)  


## Getting Started

To get started, start by going to the <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Data" target="_blank">Data</a> folder to access the heart disease dataset. Download the dataset to your local repository and keep note of the directory path for the downloaded file. For model analysis, navigate to the <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Models" target="_blank">Models</a> folder, where code files for the logistic regression, XGBoost base model, and XGBoost optimized model can be found. The code files can be followed step-by-step. Adjust working directory and file locations accordingly.

Visualizations and predictor plots can be found in the <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Visualizations" target="_blank">Visualizations</a>, <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Other%20Resources" target="_blank">Other Resources</a>, and <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Other%20Resources/Plots" target="_blank">Plots</a> folders. 

Feature importance, exploratory data analysis, and cutoff value analysis in logistic regression can be found in the <a href="https://github.com/MGT-6203-Spring-2024-Edx/Team-13/tree/main/Code" target="_blank">Code</a> folder.

## Results

If the code is run using the dataset and models provided, the result should produce the following:

Metric | XGBoost | Logistic Regression (0.5 threshold) | Optimized XGBoost
:---: | :---: | :---: | :--:
Accuracy | 90.72% | 90.79% | 83.77%
Sensitivity | 53.55% | 11.96% | 84.25%
Specificity | 91.48% | 98.91% | 83.31% 
Positive Predictive Value (PPV) | 11.34% | 53.19% | 83.06%
Negative Predictive Value (NPV) | 98.87% | 91.60% | 84.49%
Prevalence | 1.99% | 9.34% | 49.28%
Balanced Accuracy | 72.58% | 55.44% | 83.78%
AUC | 84.09% | 84.51% | 92.01%

