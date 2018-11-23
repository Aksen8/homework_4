library(tidyverse)

setwd("/Users/kristinaaksenova/econ_5100/home_practice")
murders <- read.csv('/Users/kristinaaksenova/econ_5100/home_practice/homework_4/raw_data/murders.csv')

#Subsetting. 1995 year
murders1995 <- murders %>%
    filter(year == 1995)
head(murders1995)

#Descritive statistics 
summary(murders1995)  
range(murders1995$murders)  
hist(murders1995$murders)

#Correlation coefficients
cor(murders1995$murders, murders1995$arrests)
cor(murders1995$popul, murders1995$arrests)
cor(murders1995$murders, murders1995$popul)
cor(murders1995$murdrate, murders1995$arrestrate)
cor(murders1995$percblack, murders1995$murders)


#Linear Regression. Model 1
model1 <- lm(formula = murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, 
             data = murders1995)
summary(model1)

#Standardized residuals histogram. Model 1
stand_res_model1 <- rstandard(model1)
murders1995 %>% ggplot(aes(x = stand_res_model1)) +
    geom_histogram(bins = 20) + xlab("Standardized residuals")
ggsave(here("homework_4/figures", "stand_res_model1.png"))

#Residual vs the fitted value graph. Model 1
ggplot(murders1995, aes(x = model1$fitted.values, y = model1$residuals)) +
    geom_point() 
ggsave(here("homework_4/figures", "res_fitval_model1.png"))

#Linear Regression. Model 2
model2 <- lm(formula = murdrate ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, 
             data = murders1995)
summary(model2)

#Standardized residuals histogram. Model 2
stand_res_model2 <- rstandard(model2)
murders1995 %>% ggplot(aes(x = stand_res_model2)) +
    geom_histogram(bins = 20) + xlab("Standardized residuals")
ggsave(here("homework_4/figures", "stand_res_model2.png"))

#Residual vs the fitted value graph. Model 2
ggplot(murders1995, aes(x = model2$fitted.values, y = model2$residuals)) +
    geom_point() 
ggsave(here("homework_4/figures", "res_fitval_model2.png"))

