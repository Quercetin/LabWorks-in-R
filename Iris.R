# install.packages("randomForest")
# install.packages("e1071")
# install.packages("reshape2")
# install.packages("Iris")
# Load necessary libraries
library(randomForest)
library(e1071)
library(class)
library(ggplot2)
library(reshape2)
library(dplyr)
setwd("C:/Users/RMBIO11/Documents/RSTUDIO/Iris")

# Load the Iris data
iris_data <- read.csv("iris.csv", header = TRUE)
str(iris_data)
# Graph Boxplot
boxplot(formula = Petal.Length ~ Species, data = iris, col=c("red","yellow", "blue"))
boxplot(formula = Petal.Width ~ Species, data = iris, col=c("red","yellow", "blue"))
boxplot(formula = Sepal.Length ~ Species, data = iris, col=c("red","yellow", "blue"))
boxplot(formula = Sepal.Width ~ Species, data = iris, col=c("red","yellow", "blue"))

setosa <- iris[iris$Species == "setosa", ]
virginica <- iris[iris$Species == "virginica", ]
dim(setosa)
dim(virginica)
# Shapiro test on normality:
setosaN<- as.numeric(iris[iris$Species == "setosa", "Sepal.Length"])
table(duplicated(setosaN))
ShapiroTestSetosa <- shapiro.test(setosaN)
print(ShapiroTestSetosa)
summary(ShapiroTestSetosa)

# Shapiro test on normality:
virginicaN<- as.numeric(iris[iris$Species == "virginica", "Sepal.Length"])
table(duplicated(virginicaN))

ShapiroTestVirg <- shapiro.test(virginicaN)
print(ShapiroTestVirg)
summary(ShapiroTestVirg)

# Comparition T-test Petal.Length of setosa and virginica
PairTTest<-t.test(x = setosa$Petal.Length, y = virginica$Petal.Length)
print(PairTTest)
summary(PairTTest)
# Comparition AOV Petal.Length of setosa and virginica
PetLenAOV<-aov(formula = Petal.Length ~ Species, data = iris)
print(PetLenAOV)
summary(object = PetLenAOV)
