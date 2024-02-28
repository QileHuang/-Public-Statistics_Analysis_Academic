# Notes:
# I. Using VIF to eliminate unneeded variables
# II. Segment customers according to demographic attributes

# Consumer Behavior Data Analysis
library(e1071)
library(corrplot)

# 1.0 Set-ups: Install and load the 'ggplot2' package if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 1.1Read csv data. 
setwd ("/2023 Fall/Advanced Test/Group Projects")
data <- read.csv("Mall_Customers.csv")

# 1.2 Recode Gender into numeric variable (Female = 0, Male = 1)
data$Gender <-as.numeric(factor(data$Gender))
data[2] <- data [2]-1

# 1.3 Reframe data
content <- data.frame(
  ID = data$CustomerID,
  gender = data$Gender,
  age = data$Age,
  annual_income = data$Annual.Income..k..,
  spending_score = data$Spending.Score..1.100.
)

print(content)

# 1.4 Data standardization options
switch <- 0 
if(switch == 1){
  content$gender <- scale(content$gender)
  content$age <- scale(content$age)
  content$annual_income <- scale(content$annual_income)
  content$spending_score <- scale(content$spending_score)
  
} else {
  
}

# 1.5 Summarize the statistics of sample data
summary(content)

# 1.6 Skewness 
skew_age <- skewness(content$age)
skew_annual_income <- skewness(content$annual_income)
cat("The skewness of age", skew_age)
cat("The skewness of annual income: " , skew_annual_income)

# 2.0 Correlations
# 2.1 Correlation matrix
interested_factors <- content[c("gender","age","annual_income","spending_score")]
correlation_matrix <- cor(interested_factors)
print("The correlation matrix of gender, age annual income and spending score is: ")
print(correlation_matrix)

# 2.2 Correlation plot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
  

# 3.0 Regressions
# 3.1 Single linear regression (Use age to explain spending score)
slr_model <- lm(spending_score ~ age, data = content)
summary(slr_model)

# 3.2 Multiple linear regression (Use gender, age and annual income to explain spending score)
mlr_model <- lm(spending_score ~ gender + age + annual_income, data = content)
print("Single Linear model summary:")
summary(mlr_model)

# 3.3 Observe the distribution of age
plot(content$age)
stem(content$age)

# 4.0 Hypothesis Test
# 4.1 Chi-squared tests for goodness of fit in terms of annual income. Compared to US individual income statistics in 2022 published on statista.
  #H0: The annual income for sampled mall customers is consistent with US population data.
  #H1: The annual income for sampled mall customers is not consistent with US population data.
# 4.2 Create benchmark matrix
income_benchmark <- data.frame(
  income_group = c("Under 25,000",
                   "25,000 to 34,999",
                   "35,000 to 49,999",
                   "50,000 to 74,999",
                   "75,000 to 99,999",
                   "100,000 and over"
                   ),
  expected_data = c(31.4,
                    15.2,
                    21.2,
                    32.4,
                    24.6,
                    75
                            )
)
print(income_benchmark)

# 4.3 Cut mall customer data into corresponding bins
bin_intervals <- c(0,25,35,50,75,100, Inf)
content$income_category <- cut(content$annual_income, breaks = bin_intervals,labels = c(
                  "Under 25,000",
                   "25,000 to 34,999",
                   "35,000 to 49,999",
                   "50,000 to 74,999",
                   "75,000 to 99,999",
                   "100,000 and over"), include.lowest = TRUE, right = FALSE)
category_count <- table(content$income_category)
observed_data <- as.data.frame(category_count)
colnames(observed_data) <- c("Income Categories", "Observations")
print(observed_data)


chi_squared_result <- chisq.test(table(content$income_category), p = income_benchmark$expected_data/sum(income_benchmark$expected_data))
test_statistic <- chi_squared_result$statistic
print(chi_squared_result)
cat("Chi-Squared Statistics: ", test_statistic)

critical_value <- qchisq(0.95,8)
print(critical_value)

if(test_statistic > critical_value){
  cat("Reject H0")
}else{
  cat("Fail to reject H0")
}


# 5.0 Probability Test
# 5.1 Observe the distribution of age and spending scores
x <- content$age
y <- content$spending_score

ggplot(data.frame(x = x, y = y), aes(x, y)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot Example", x = "age", y = "annual_income")

# 5.2 Formulate a matrix of variable inputs
customers_age <- data.frame(age = c(25,35,45))

# 5.3 Prediction and visualization
prediction <- predict(slr_model,newdata = customers_age, interval = "prediction", level = 0.95)
rownames(prediction) <- c("predicted value","lower bounds", "upper bounds")
print("The prediction of the spending scores of customers aging 25, 35, and 45 is: ") 
print(prediction)