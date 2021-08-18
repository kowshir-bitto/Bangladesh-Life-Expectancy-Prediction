library("readxl")
library(tidyverse)
library(ggplot2)
library(GGally)
library(caret)
library(car)
library(neuralnet)
data<-read_excel("C:/Users/Bitto/Desktop/South Asia Life & GDP/Data/Bangladesh.xlsx")
data
data <- data %>%
  select(-Country, -Region ,- Income_Group)
data
cor(data)
ggpairs(data, title = "Scatterplot Matrix of the data Set")
#Chi<-table(data$ Population,data$Life_Expectancy)
#Chi
#test <- chisq.test(chi)
#test 
#Chi1<-table(data$ GDP,data$Life_Expectancy)
#Chi1
#test <- chisq.test(chi1)
#test 
ggplot(data, aes(x = Year, y = Life_Expectancy)) + geom_point()+stat_smooth(method=lm)
simple_regression<-lm(Life_Expectancy~Year,data)
summary(simple_regression)
plot(simple_regression)
ggplot(data, aes(x = GDP, y = Life_Expectancy)) + geom_point()+stat_smooth(method=lm)
simple_regression1<-lm(Life_Expectancy~GDP,data)
summary(simple_regression1)
plot(simple_regression1)
ggplot(data, aes(x = Population, y = Life_Expectancy)) + geom_point()+stat_smooth(method=lm)
simple_regression2<-lm(Life_Expectancy~Population,data)
summary(simple_regression2)
plot(simple_regression2)
multiple_regression<-lm(Life_Expectancy~GDP+Year+Population,data)
summary(multiple_regression)
plot(multiple_regression)
vif(multiple_regression)
corMatrix<-round(cor(data))
findCorrelation(corMatrix, cutoff = 0.7, names = TRUE)
multiple_regression1<-lm(Life_Expectancy~GDP+Population,data)
summary(multiple_regression1)
plot(multiple_regression1)
vif(multiple_regression1)
data1 <- data %>%
  select(-Country, -Region ,- Income_Group)
data1
scale01 <- function(x){(x - min(x)) / (max(x) - min(x))}
data <- data1 %>%
  mutate_all(scale01)
data1_Train <- sample_frac(data1, replace = FALSE, size = 0.80)
data1_Test <- anti_join(data1, data1_Train)
set.seed(12345)
data_Train <- sample_frac(data, replace = FALSE, size = 0.80)
data_Test <- anti_join(data, data_Train)
set.seed(12321)
NN1 <- neuralnet(Life_Expectancy~GDP+Year,data = data_Train)
plot(NN1, rep = 'best')
multiple_regression<-lm(Life_Expectancy~GDP+Year,data)
summary(multiple_regression)
plot(multiple_regression)
vif(multiple_regression)
NN1_Train_SSE <- sum((data.frame(NN1$net.result) - data_Train[, 1])^2)/2
paste("SSE: ", round(NN1_Train_SSE, 4))
Test_NN1_Output <- compute(NN1, data_Test[1, 1:3])$net.result
NN1_Test_SSE <- sum((Test_NN1_Output - data_Test[, 1])^2)/2
NN1_Test_SSE
Test_multiple_regression_Output <- predict(multiple_regression, data_Test)
multiple_regression_Test_SSE <- sum((Test_multiple_regression_Output - data_Test[, 1])^2)/2
multiple_regression_Test_SSE
library(Metrics)
NN1_Test_RMSE <- rmse(Test_NN1_Output,data_Test[,1])
NN1_Test_RMSE
multiple_regression_Test_RMSE <- rmse(Test_multiple_regression_Output,data_Test[,1])
