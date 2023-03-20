# Simple linear regression with boston housing
library(tidyverse)

data = read_csv("heart.data.csv")
predict_data = read_csv("Heart_validation.csv")


# Explore data

head(data)
glimpse(data)
length(data)
summary(data)

# Missing values
colSums(is.na(data))

# Biking
ggplot(data=data, aes(biking)) + geom_density()

# Mean for imputation - biking
biking_mean = mean(data$biking, na.rm = TRUE)
data$biking = ifelse(is.na(data$biking), biking_mean, data$biking)
colSums(is.na(data))

# Mean for imputation - smoking
ggplot(data=data, aes(smoking)) + geom_density()
smoking_mean = mean(data$smoking, na.rm = TRUE)
data$smoking = ifelse(is.na(data$smoking), smoking_mean, data$smoking)
colSums(is.na(data))

# Mean for imputation - heart.disease
ggplot(data=data, aes(heart.disease)) + geom_density()
heart.disease_mean = mean(data$heart.disease, na.rm = TRUE)
data$heart.disease = ifelse(is.na(data$heart.disease), heart.disease_mean, data$heart.disease)
colSums(is.na(data))

# Simple Linear regression
data_new = data[c(1,3)] # Extracting column with index value
glimpse(data_new)

# Train test split
install.packages('caTools')
library(caTools)
set.seed(123)

split = sample.split(data$heart.disease, SplitRatio = 1/4)
training = subset(data, split=TRUE)
test = subset(data, split=FALSE)

# Fit our model on training set
regressor = lm(formula=heart.disease~biking, training)
summary(regressor)

# Predict
new = predict_data
paste("The value is: ",predict(regressor, newdata = new))
y_pred = predict(regressor, newdata = test)

# Viusalization
ggplot()+geom_point(aes(x=test$biking, y=test$heart.disease), color='red')+
  geom_line(aes(x=test$biking, y=y_pred), color='blue')+
  xlab('biking')+
  ylab('heart disease')

# Mean Squared Error (1/n * summation(actual - predicted)^2)
summ = summary(regressor)
summ
paste("The mean squared error is: ", mean(summ$residuals^2))



# Mean Squared error is 2.69
# R squared is 0.87
# Actual heart disease values were 9.6, 13.5, 10.1, 11.8
# Predicted heart disease values were 8.7, 16.0, 9.3, 11.6

