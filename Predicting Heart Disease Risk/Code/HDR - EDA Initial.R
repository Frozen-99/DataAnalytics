library(ggplot2)
library(corrplot)
library(dplyr)

plot_location <- 'Other Resources/Plots/'
data <- read.csv("Data/heart_disease_health_indicators_BRFSS2015.csv")

names(data)[names(data) == "Age"] <- "AgeGroup"
names(data)[names(data) == "Income"] <- "IncomeGroup"

count(filter(data, HeartDiseaseorAttack == 0))

head(data)

# correlation heatmap
correlation_matrix <- cor(data)
corrplot_heatmap <- corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
filename = paste(plot_location, "correlation_heatmap.png", sep = "")
# print(corrplot_heatmap)



# histogram of age group
histogram <-ggplot(data, aes(x = AgeGroup)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age Group (1 (18-22) through 14)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

filename = paste(plot_location, "histogram.png", sep = "")
print(histogram)
ggsave(filename, plot = histogram, width = 6, height = 4, dpi = 300, bg = "white")

# bar plots for binary variables
create_binary_bar_plot <- function(variable, filename) {
  proportions <- prop.table(table(data[[variable]], data$HeartDiseaseorAttack), margin = 1)
  proportions_df <- as.data.frame(proportions)
  
  b_plot <- ggplot(proportions_df, aes(x = Var1, y = Freq, fill = factor(Var2))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Heart Attack Risk w/", variable),
         x = variable,
         y = "Proportion",
         fill = "Heart Attack Risk") +
    scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("No", "Yes")) +
    theme_minimal() + 
    theme(panel.background = element_rect(fill = "white"))
  
  ggsave(filename, b_plot, width = 6, height = 4, dpi = 300)
  
  return(b_plot)
}

# bar plots for binary variables

binary_variables <- c("HighBP", "HighChol", "Sex", "AgeGroup", "Stroke", "Diabetes", "PhysActivity", 
                      "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare" )

for (variable in binary_variables) {
  filename <- paste( plot_location , variable, ".png", sep = "")
  b_plot <- create_binary_bar_plot(variable, filename)
  print(b_plot)
}

# plot <- create_binary_bar_plot("AgeGroup")
# print(plot)


create_continuous_histogram <- function(variable, filename) {
  c_plot <- ggplot(data, aes(x = data[[variable]])) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    labs(title = paste("Distribution of", variable),
         x = variable,
         y = "Frequency") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"))
  
  ggsave(filename, c_plot, width = 6, height = 4, dpi = 300)
  return(c_plot)
}

# histograms for continuous variables
continuous_variables <- c("BMI",  "MentHlth", "PhysHlth", "DiffWalk")

for (variable in continuous_variables) {
  filename <- paste(plot_location , variable, ".png", sep = "")
  c_plot <- create_continuous_histogram(variable, filename)
  print(c_plot)
}

create_categorical_bar_plot <- function(variable, filename) {
  proportions <- prop.table(table(data[[variable]], data$HeartDiseaseorAttack), margin = 1)
  proportions_df <- as.data.frame(proportions)
  
  cat_plot <- ggplot(proportions_df, aes(x = Var1, y = Freq, fill = factor(Var2))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Heart Attack Risk by", variable),
         x = variable,
         y = "Proportion",
         fill = "Heart Attack Risk") +
    scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("No", "Yes")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"))
  
  ggsave(filename, cat_plot, width = 6, height = 4, dpi = 300)
  return(cat_plot)
}

# bar plots for categorical variables
categorical_variables <- c("GenHlth", "Sex", "AgeGroup", "Education", "IncomeGroup")

for (variable in categorical_variables) {
  filename <- paste( plot_location , variable, ".png", sep = "")
  cat_plot <- create_categorical_bar_plot(variable, filename)
  print(cat_plot)
}