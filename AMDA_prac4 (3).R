# practical 4
# 1) view dataset
View(iris)
str(iris)

# 2) draw Scatter plots aong with the correlation coefficients for each pair of variabes 
# other than Species"

library(psych)
pairs.panels(iris[1:4],gap=0,bg=c("red","green","purple")[iris$Species],pch=21)

## partition data into train (60%) and test (40%)
set.seed(555)
ind=sample(2,nrow(iris),replace = T,prob = c(0.6,0.4))
training=iris[ind==1,]
testing=iris[ind==2,]

# 4) Run discriminant analysis on train data with species as dependent variable 
library(biotools)
res=boxM(iris[,1:4],iris[,"Species"]) # used to compare variation
# bartlets test for homogenity of variance 
# HO: population variance are equal
bartlett.test(c(iris$Sepal.Length+iris$Sepal.Width+iris$Petal.Length+iris$Petal.Width)~iris$Species)

# pvlaue is <0.05
# Reject HO : i.e population variance are not equal

library(MASS)
#LDA (linear discriminant analysis)
linear=lda(Species~.,training)
linear

# in output , 2 equations are given:
linear$prior
linear$counts
linear$scaling

p=predict(linear,training)
p

# 5) Draw histogram to check how well discriminant functions seperate the three classes

ldahist(data=p$x[,1],g=training$Species)
ldahist(data=p$x[,2],g=training$Species)

# First graph, idal is better helping us distinguish all 3 species

#6) obtain the confusion matrix and calculate accuracy for train as well as test data

/*
library(caret)
library(e1071)
confusionmatrix(p$class,training$Species)

p1=predict(linear,training)$class
tab=table(predicted=p1,actual=training$species)
tab

sum(diag(tab))/sum(tab) # accuracy

p2=predict(linear,training)$class
tab2=table(Predicted=p2,Actual=tarining$species)
tab2

sum(diag(tab2))/sum(tab2) 

*/





