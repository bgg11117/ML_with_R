#linear regression(CART - regression trees , model trees)
library(rpart) #CART
library(rpart.plot) #produces a tree diagram
library(RWeka) # model trees , M5 algorithm

wine <- read.csv('whitewines.csv')
#exploring data
str(wine)
#one of the advantages of trees is that they can handle many types of data 
#without preprocessing. This means we do not need to normalize or standardize the features.
hist(wine$quality)
summary(wine$quality) #確定data is reliable

#因為data 已經隨機排序 ，可以直接分割兩份 (3:1)
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

#training a model
m.rpart <- rpart(quality ~ .,data = wine_train)
m.rpart
#For instance, all 3,750 examples begin at the root node, of which 2,372 have
#alcohol < 10.85 and 1,378 have alcohol >= 10.85. Because alcohol was used
#first in the tree, it is the single most important predictor of wine quality.

#Nodes indicated by * are terminal or leaf nodes, which means that they result in a
#prediction (listed here as yval).
summary(m.rpart)

#visualizing decision trees
rpart.plot(m.rpart,digits=3)
rpart.plot(m.rpart , digits = 4 , fallen.leaves = TRUE , type =3 , extra =101)
#The fallen.leaves parameter forces the leaf nodes to be aligned
#at the bottom of the plot, while the type and extra parameters 
#affect the way the decisions and nodes are labeled.

#evaluating model performance
p.rpart <- predict(m.rpart , wine_test)
summary(p.rpart)
summary(wine_test$quality)
#This finding suggests that the model is not correctly identifying the extreme cases, 
#in particular the best and worst wines. On the other hand, 
#between the first and third quartile, we may be doing well.

cor(p.rpart , wine_test$quality)
#A correlation of 0.54 is certainly acceptable. However, the correlation only measures
#how strongly the predictions are related to the true value; it is not a measure of how
#far off the predictions were from the true values.

#measuring performance with the mean absolute error(MAE)
MAE <- function(actual,predicted){
  mean(abs(actual-predicted))
}

MAE(p.rpart , wine_test$quality)
#This implies that, on average, the difference between our model's predictions and the
#true quality score was about 0.59. On a quality scale from zero to 10, this seems to
#suggest that our model is doing fairly well.
mean(wine_train$quality)

MAE(5.87, wine_test$quality) #If we predicted the value 5.87 for every wine sample, 
#we would have a mean absolute error of only about 0.67

#Our regression tree (MAE = 0.59) comes closer on average to the true quality score
#than the imputed mean (MAE = 0.67), but not by much. In comparison, Cortez
#reported an MAE of 0.58 for the neural network model and an MAE of 0.45 for the
#support vector machine. This suggests that there is room for improvement.

#improving model performance
m.m5p <- M5P(quality~., data = wine_train)
m.m5p
#A key difference, however, is that the nodes terminate not
#in a numeric prediction, but a linear model (shown here as LM1 and LM2).
summary(m.m5p)

p.m5p <- predict(m.m5p , wine_test)
summary(p.m5p)
cor(p.m5p , wine_test$quality) #The correlation also seems to be substantially higher
MAE(p.m5p , wine_test$quality) #The model has slightly reduced the mean absolute error

