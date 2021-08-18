library("readxl")
library(tidyverse)
library(ggplot2)
library(GGally)
library(caret)
library(car)
library(neuralnet)
library(Metrics)
data<-read_excel("H:/Research/South Asia Life & GDP/Data/Bangladesh.xlsx")
data
ggpairs(data, title = "Scatterplot Matrix of the data Set")
ggplot(data, aes(x = GDP + Population, y = Life_Expectancy)) + geom_point()+stat_smooth(method=lm)
multiple_regression<-lm(Life_Expectancy~GDP+Population,data)
summary(multiple_regression)
plot(multiple_regression)
vif(multiple_regression)
data1 <- data %>%
  select(-Country, -Region ,- Income_Group,-Year)
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
NN1 <- neuralnet(Life_Expectancy~GDP+Population,data = data_Train)
plot(NN1, rep = 'best')
cor(data)

nn <- neuralnet(Life_Expectancy~GDP+Population, 
                data = data_Train, hidden = c(5, 3), 
                linear.output = TRUE)
pr.nn <- compute(nn, data_Test[,1:3])

pr.nn_ <- pr.nn$net.result * (max(data$Life_Expectancy) - min(data$Life_Expectancy)) 
                                              + min(data$Life_Expectancy)
test.r <- (data_Test$Life_Expectancy) * (max(data$Life_Expectancy) - min(data$Life_Expectancy)) + 
                                              min(data$Life_Expectancy)

RMSE.nn = (sum((data1_Test$Life_Expectancy- data_Test)^2) / nrow(data1_Test)) ^ 0.5
plot(nn)
plot(data_Test$Life_Expectancy, pr.nn_, col = "red", 
     main = 'Real vs Predicted')
abline(0, 1, lwd = 2)
