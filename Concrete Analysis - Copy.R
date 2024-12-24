# libraries
library(readxl)         
library(ggplot2)      
library(dplyr)         
library(corrplot)     
library(car)          
library(MASS)        
library(psych)          
library(tidyr) 
library(glmnet)

# The concrete dataset
Concrete <- read_excel("C:/Users/user/Desktop/Assignment Project/Task 2/concrete compressive strength.xlsx")
names(Concrete)

# Viewing the concrete dataset

View(Concrete)

# EDA (Exploratory Data Analysis)

# Total number of missing values that might be in the dataset
sum(is.na(Concrete))

# The structure of the dataset
str(Concrete)

# Descriptive statistics 
summary(data)
describe(data)


# the column "Concrete Category" is a character and
# the column "Contains Fly Ash" is Logical

unique_ConCat <- unique(Concrete$`Concrete Category`)
unique_ConCat


FlyAsh <- unique(Concrete$`Contains Fly Ash`)
FlyAsh


# Rename all columns at once
colnames(Concrete) <- c("Cement", "Blast Furnace Slag", "Fly Ash", "Water", "Superplasticizer", "Coarse Aggregate", "Fine Aggregate", "Age (day)", "Concrete Category", "Contains Fly Ash", "Concrete compressive strength")

#listing the column names
names(Concrete)

#Visualizations
#_____________________________________

# Separating the numeric, character, and logical columns

char <- sapply(Concrete, is.character)
logi <- sapply(Concrete, is.logical)
num <- sapply(Concrete, is.numeric)

# Setting up plotting parameters for larger plots and larger titles
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1) 

# Histograms for each numeric variable to check distributions
for (col in names(Concrete)[num]) {
  hist(Concrete[[col]], 
       main = paste("Histogram of", col), 
       xlab = col, 
       col = "lightblue", 
       border = "black", 
       cex.main = 1.0,         
       cex.axis = 1.2,    
       cex.lab = 1.3      
  )
}



# Bar plots for each character variable
for (col in names(Concrete)[char]) {
  barplot(table(Concrete[[col]]), 
          main = paste("Bar Plot of", col), 
          col = "coral", 
          border = "black",
          cex.main = 1.5,      
          cex.axis = 1.2,      
          cex.names = 1.2   
  )
}

# Bar plots for each logical variable
for (col in names(Concrete)[logi]) {
  barplot(table(Concrete[[col]]), 
          main = paste("Bar Plot of", col), 
          col = "lightgreen", 
          border = "black",
          cex.main = 1.5,     
          cex.axis = 1.2,     
          cex.names = 1.2   
  )
}



# Boxplots for each variable to detect outliers for numeric columns

# parameters for large individual box plots and title space
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1) 

# Boxplots for each numeric variable to detect outliers
for (col in names(Concrete)[num]) {
  boxplot(Concrete[[col]], 
          main = paste("Boxplot of", col), 
          xlab = col, 
          col = "orange", 
          border = "brown",
          cex.main = 1.0, 
          cex.axis = 1.2, 
          cex.lab = 1.3 
  )
}


# Selecting only the numeric columns from the Concrete dataset
num <- Concrete[sapply(Concrete, is.numeric)]

# Calculating the correlation matrix
correlation_matrix <- cor(num, use = "complete.obs")

# Plot the correlation matrix as a heatmap with smaller, less bold correlation values
corrplot(correlation_matrix, 
         method = "color", 
         type = "lower", 
         tl.cex = 0.6,           
         addCoef.col = "darkorange",  
         number.cex = 0.7,        
         addCoefasPercent = FALSE
)

# Histogram of Cement with legend for color
ggplot(Concrete, aes(x = `Cement`)) + 
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Cement Content",
       x = "Cement",
       y = "Frequency",
       fill = "Cement Content") +
  theme(legend.position = "right")




# Extracting significant correlations
correlations <- as.data.frame(as.table(correlation_matrix))
significant_correlations <- correlations %>% filter(abs(Freq) > 0.5 & Freq != 1)
print(significant_correlations)


# Calculating and interpreting correlations between input variables and compressive strength
cor_strength <- cor(num[ , -9], Concrete$`Concrete compressive strength`)
print(cor_strength)


# Linear regression model
lm_model <- lm(`Concrete compressive strength` ~ `Cement` + `Blast Furnace Slag` 
               + `Fly Ash` + `Water` + `Superplasticizer` 
               + `Coarse Aggregate` + `Fine Aggregate` + `Age (day)`, data = Concrete)
summary(lm_model)



# Diagnostic plots for assumptions
# Residuals vs Fitted
plot(lm_model$fitted.values, lm_model$residuals, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Scatter plot with colorful points
plot(lm_model$fitted.values, lm_model$residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 16,  # Change point type to filled circles
     col = heat.colors(length(lm_model$residuals))) 
abline(h = 0, col = "red")



# QQ Plot for normality of residuals
qqnorm(lm_model$residuals)
qqline(lm_model$residuals, col = "blue")



# QQ Plot for normality of residuals with colorful points
qqnorm(lm_model$residuals, main = "QQ Plot for Residuals", pch = 16, 
       col = rainbow(length(lm_model$residuals)))
qqline(lm_model$residuals, col = "blue")

# Scale-Location plot for homoscedasticity
plot(lm_model$fitted.values, sqrt(abs(lm_model$residuals)), main = "Scale-Location Plot")

plot(lm_model$fitted.values, sqrt(abs(lm_model$residuals)), 
     main = "Scale-Location Plot", 
     xlab = "Fitted Values", 
     ylab = "Sqrt |Residuals|", 
     pch = 16,  # Filled circles
     col = heat.colors(length(lm_model$residuals))) 

# Advanced Regression (Lasso and Ridge)
# Preparing data for Ridge and Lasso regression
x <- as.matrix(num[, -ncol(num)])
y <- Concrete$`Concrete compressive strength`

# Ridge Regression
ridge_model <- glmnet(x, y, alpha = 0)
cv_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
ridge_model_best <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
coef(ridge_model_best)

# Lasso Regression
lasso_model <- glmnet(x, y, alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
lasso_model_best <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
coef(lasso_model_best)



#--- Hypothesis Testing

#  Testing the Effect of Cement

# Hypothesis 1: Does Cement content affect Compressive Strength?
cor_test_cement <- cor.test(Concrete$`Cement`, Concrete$`Concrete compressive strength`)
print(cor_test_cement)


# Categorize the cement component into two groups: Low and High
Concrete$Cement_Group <- ifelse(Concrete$`Cement` > median(Concrete$`Cement`, na.rm = TRUE), "High", "Low")

t_test_cement <- t.test(Concrete$`Concrete compressive strength` ~ Concrete$Cement_Group)
print(t_test_cement)


# Hypothesis 2: Does Water content affect Compressive Strength?
Concrete$WaterGroup <- ifelse(Concrete$`Water` > median(Concrete$`Water`, na.rm = TRUE), "High", "Low")

# Conducting the t-test using the new binary grouping variable
t_test_water <- t.test(Concrete$`Concrete compressive strength` ~ Concrete$WaterGroup)
print(t_test_water)


# Performing a Pearson correlation test between water content and compressive strength
cor_test_water <- cor.test(Concrete$`Concrete compressive strength`, 
                           Concrete$`Water`, 
                           method = "pearson")
print(cor_test_water)

# Running a linear regression with compressive strength as the response and water as the predictor
regression_water <- lm(`Concrete compressive strength` ~ `Water`, data = Concrete)
summary(regression_water)




# Performing a Pearson correlation test between superplasticizer content and compressive strength
cor_test_superplasticizer <- cor.test(Concrete$`Concrete compressive strength`, 
                                      Concrete$`Superplasticizer`, 
                                      method = "pearson")
print(cor_test_superplasticizer)

# Running a linear regression with compressive strength as the response and superplasticizer content as the predictor
regression_superplasticizer <- lm(`Concrete compressive strength` ~ `Superplasticizer`, data = Concrete)
summary(regression_superplasticizer)

