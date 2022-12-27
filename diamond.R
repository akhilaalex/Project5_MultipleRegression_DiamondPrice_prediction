#importing libraries
library(ggplot2)
library(tidyverse)
library(ISLR2)
library(stargazer)
library(caret)
library(leaps)
library(MASS)

#importing dataset

diamond=read.csv("C:\\Users\\ALAN\\Desktop\\conestogac\\multivariate_statistics\\DiamondsPrices.csv")
view(diamond)

summary(diamond)
summary(diamond$carat)

diamond %>% ggplot(aes(price)) + geom_histogram()
hist(diamond$price)

diamond %>% ggplot(aes(carat)) + geom_histogram()
hist(diamond$carat)

diamond %>% ggplot(aes(depth)) + geom_histogram()
hist(diamond$depth)

cor(diamond$price, diamond$carat)
cor(diamond$price, diamond$depth)

diamond %>% ggplot(aes(x = carat, y = price)) + geom_point() + geom_smooth(method="lm", se=FALSE)
diamond %>% ggplot(aes(x = depth, y = price)) + geom_point() + geom_smooth(method="lm", se=FALSE)

diamond %>% ggplot(aes(x = color, y = price)) + geom_boxplot()
diamond %>% ggplot(aes(x = clarity, y = price)) + geom_boxplot()

carat_regression <- lm(price ~ carat, data = diamond)
coef(carat_regression)

depth_regression <- lm(price ~ depth, data = diamond)
coef(depth_regression)

carat_predictions <- data.frame(carat = c(.78, .40, .8))
predict(carat_regression, carat_predictions)

depth_predictions <- data.frame(depth = c(10.4, 40.7, 80.9))
predict(depth_regression, depth_predictions)

first_regression <- lm(price ~ carat+color, data = diamond)
coef(first_regression)


second_regression <- lm(price ~ carat+clarity, data = diamond)
coef(second_regression)


third_regression <- lm(price ~ carat+clarity+color+depth, data = diamond)
coef(third_regression)

fourth_regression <- lm(price ~ carat+depth, data = diamond)
coef(fourth_regression)
fourth_prediction <- data.frame(carat = c(.34, .57), depth = c(54, 40))
predict(fourth_regression,fourth_prediction)

fifth_regression<-lm(price ~ carat+depth+color+cut+clarity, data = diamond)
coef(fifth_regression)

fullmodel<- lm(price ~ ., data = diamond)
coef(fullmodel)


carat_regression_Resids <- carat_regression $residuals
carat_regression_Fitted <-carat_regression $fitted.values
hist(carat_regression_Resids)
qqnorm(carat_regression_Resids)
plot(carat_regression_Fitted,carat_regression_Resids)

fifth_regression_Resids <- fifth_regression$residuals
fifth_regression_Fitted <-fifth_regression $fitted.values
hist(fifth_regression_Resids)
qqnorm(fifth_regression_Resids)
plot(fifth_regression_Fitted,fifth_regression_Resids)



fullmodel_Resids <- fullmodel $residuals
fullmodel_Fitted <-fullmodel $fitted.values
hist(fullmodel_Resids)
qqnorm(fullmodel_Resids)
plot(fullmodel_Fitted,fullmodel_Resids)

caratSqrModel <- lm(price  ~ carat+I(carat^2), data = diamond)
coef(caratSqrModel)

carat_predictionssqr <- data.frame(carat = c(.78, .40, .8))
predict(caratSqrModel, carat_predictionssqr)


depth_regressionsqr <- lm(price ~ depth+I(depth^2), data = diamond)
coef(depth_regressionsqr)


fifthSqrModel <- lm(price  ~ carat+I(carat^2)+depth+color+cut+clarity, data = diamond)
coef(fifthSqrModel)

fullmodel<- lm(price ~ ., data = diamond)
step<-stepAIC(fullmodel,trace='false') 
step$anova

carat_regression <- lm(price ~ carat, data = diamond)
step1<-stepAIC(carat_regression,trace='false') 
step1$anova

depth_regression <- lm(price ~ depth, data = diamond)
step2<-stepAIC(depth_regression,trace='false') 
step2$anova

cut_regression <- lm(price ~ cut, data = diamond)
step3<-stepAIC(cut_regression,trace='false') 
step3$anova

color_regression <- lm(price ~ color, data = diamond)
step4<-stepAIC(color_regression,trace='false') 
step4$anova

clarity_regression <- lm(price ~clarity, data = diamond)
step5<-stepAIC(clarity_regression,trace='false') 
step5$anova

ccolor_regression <- lm(price ~ carat+color, data = diamond)
step6<-stepAIC(ccolor_regression,trace='false') 
step6$anova

cdepth_regression <- lm(price ~ carat+depth, data = diamond)
step7<-stepAIC(cdepth_regression,trace='false') 
step7$anova

cclarity_regression <- lm(price ~ carat+clarity, data = diamond)
step8<-stepAIC(cclarity_regression,trace='false') 
step8$anova

ccut_regression <- lm(price ~ carat+cut, data = diamond)
step9<-stepAIC(ccut_regression,trace='false') 
step9$anova

cclaritydepth_regression <- lm(price ~ carat+clarity+depth, data = diamond)
step10<-stepAIC(cclaritydepth_regression,trace='false') 
step10$anova

cclaritycut_regression <- lm(price ~ carat+clarity+cut, data = diamond)
step11<-stepAIC(cclaritycut_regression,trace='false') 
step11$anova

cclaritycolor_regression <- lm(price ~ carat+clarity+color, data = diamond)
step12<-stepAIC(cclaritycolor_regression,trace='false') 
step12$anova

cclaritycolordepth_regression <- lm(price ~ carat+clarity+color+depth, data = diamond)
step13<-stepAIC(cclaritycolordepth_regression,trace='false') 
step13$anova

cclaritycolorcut_regression <- lm(price ~ carat+clarity+color+cut, data = diamond)
step14<-stepAIC(cclaritycolorcut_regression,trace='false') 
step14$anova

cclaritycolorcutdepth_regression <- lm(price ~ carat+clarity+color+cut+depth, data = diamond)
step15<-stepAIC(cclaritycolorcutdepth_regression,trace='false') 
step15$anova

summary(cclaritycolorcutdepth_regression )

cclaritycolorcutdepth_regression <- lm(price ~ carat+clarity+color+cut+depth, data = diamond)
coef(cclaritycolorcutdepth_regression)

cclaritycolorcutdepth_predictions <- data.frame(carat = .99,clarity='SI1',color='j',cut='Very Good',depth=62.4)
predict(carat_regression,cclaritycolorcutdepth_predictions)


subsetmodel<-regsubsets(price~.,data=diamond)
plot(subsetmodel, scale = "adjr2")
