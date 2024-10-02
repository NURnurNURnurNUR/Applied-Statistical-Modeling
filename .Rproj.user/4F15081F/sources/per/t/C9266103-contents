# Load the libraries 
library('tidyverse')
library('ggplot2')
library(dplyr)
library(ROSE)

# Load the dataset
data <- read.csv("C:/Users/Admin/Downloads/attr/HR.csv", header= T, na.strings = '')

# Display the first few rows
head(data)

#Determine the class of dataset 
class(data)

summary(data)

# Convert relevant columns to factors
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$Attrition <- as.factor(data$Attrition)
data$OverTime <- as.factor(data$OverTime)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)

#Check the structure of data
str(data)

# Check for missing values
colSums(is.na(data))

# Summary statistics of the dataset
summary(data)
 
# Check for the outliers
columns_with_potential_outliers <- c("MonthlyIncome", "TotalWorkingYears", "YearsAtCompany", 
                                     "YearsInCurrentRole", "YearsSinceLastPromotion", 
                                     "DailyRate", "NumCompaniesWorked", "TrainingTimesLastYear")

# Check if columns exist in the dataset to avoid errors
columns_with_potential_outliers <- columns_with_potential_outliers[columns_with_potential_outliers %in% colnames(data)]

# Draw histograms for these specific columns
par(mfrow = c(2, 4)) 
for (col in columns_with_potential_outliers) {
  hist(data[[col]], 
       main = paste("Histogram of", col), 
       xlab = col, 
       col = "lightblue", 
       border = "black", 
       breaks = 20) 
}

install.packages("dplyr")
install.packages("ROSE")



# View the initial class distribution
cat("Initial class distribution:\n")
print(table(data$class))

# Use ROSE to balance the dataset
balanced_data_rose <- ROSE(class ~ ., data = data, seed = 1)$data

# View the balanced dataset's class distribution
cat("Balanced class distribution:\n")
print(table(balanced_data_rose$class))

data$Attrition <- as.factor(data$Attrition)


if ("JobSatisfaction" %in% colnames(data)) {
  
  clean_data <- na.omit(data[, c("JobSatisfaction", "Attrition")])  
  # Perform the T-test using the cleaned data
  t_test_result <- t.test(JobSatisfaction ~ Attrition, data = clean_data)
  
  
  print(t_test_result)
} else {
  print("JobSatisfaction column not found. Please check the column names.")
}

ggplot(data, aes(x = JobSatisfaction)) + 
  geom_bar(fill = "steelblue") +
  theme_minimal()

