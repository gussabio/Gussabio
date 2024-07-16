# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('insurance.csv')

# Encoding categorical data
dataset$smoker = factor(dataset$smoker,
                       levels = c('yes', 'no'),
                       labels = c(1,2))

dataset$region = factor(dataset$region,
                        levels = c('northest', 'northwest','southeast','southwest'),
                        labels = c(1,2,3,4))

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
#Backward Elimination
regressor <- lm(formula = charges ~ age + sex + bmi + children + smoker + region, data = training_set)
summary(regressor)
#Eliminate the Higher PValue region
regressor <- lm(formula = charges ~ age + sex + bmi + children + smoker, data = training_set)
summary(regressor)
#Eliminate the Higher PValue now sex
regressor <- lm(formula = charges ~ age + bmi + children + smoker, data = training_set)
summary(regressor)

regressor <- lm(formula = charges ~ age + bmi + smoker, data = training_set)
summary(regressor)

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)
y_pred