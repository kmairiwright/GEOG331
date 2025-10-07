## script to see examples of linear regression 
## in R using built in IRIS data
## KW 10/07/2025

rm(list=ls())

#subset the virginica species
flower<-iris[iris$Species=="virginica",]

#make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Iris virginica")

#fit a regression model
fit<-lm(flower$Petal.Length ~ flower$Sepal.Length)

#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab="Sepal Length", ylab = "Residuals")
abline(h=0)

##check assumptions of regression model 
#(normally distributed? Linear relationship? Equal variance?)

#check normality of residuals
hist(summary(fit)$residuals, col = "red",
     main = "Residual Distribution", xlab = "Residuals")

#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

#user Shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)
#if p value is less than .05 than data 
#is significantly less than normal distribution