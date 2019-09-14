#decision tree (tree)
library(C50) #tree package
library(gmodels)

#exploring data
credit <- read.csv('credit.csv')
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

#data preparation - creating random training & testing datasets
set.seed(123)
train_sample <- sample(1000,900) #因為萬一照順序取，可能會有bug(EX:前面都小，後面都大)
str(train_sample)

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

#確認是否平均樣本分配
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# training model (C5.0 models require a factor outcome)
credit_model <- C5.0(credit_train[-21], factor(credit_train$default)) #default在21行
credit_model 
summary(credit_model) # see the tree's decisions #1:no , 2:yes
#The Errors output notes that the model correctly classified all but 135 of the
#900 training instances for an error rate of 14.8 percent. A total of 44 actual no
#values were incorrectly classified as yes (false positives), while 91 yes values
#were misclassified as no (false negatives).

#Decision trees are known for having a tendency to overfit the model to the training
#data. For this reason, the error rate reported on training data may be overly
#optimistic, and it is especially important to evaluate decision trees on a test dataset.

#evaluating model
credit_pred <- predict(credit_model,credit_test)
CrossTable(credit_test$default , credit_pred , prop.chisq = FALSE , 
           prop.c = FALSE , prop.r =FALSE , dnn = c('actual dafault','predicted dafault'))
#Out of the 100 test loan application records, our model correctly predicted that
#60 did not default and 14 did default, resulting in an accuracy of 74 percent and
#an error rate of 26 percent.
#Note that the model only correctly predicted 14 of the 33 actual
#loan defaults in the test data, or 42 percent. Unfortunately, this type of error is a
#potentially very costly mistake, as the bank loses money on each default. 
#Let's see if we can improve the result with a bit more effort.

#improving model
credit_boost10 <- C5.0(credit_train[-21],factor(credit_train$default) , trials = 10)
credit_boost10
summary(credit_boost10) #only 29 errors on training set

credit_boost_pred10 <- predict(credit_boost10,credit_test)
CrossTable(credit_test$default , credit_boost_pred10 , prop.chisq = FALSE , 
           prop.c = FALSE , prop.r =FALSE , dnn = c('actual dafault','predicted dafault'))
#Here, we reduced the total error rate from 26 percent prior to boosting down to
#24 percent in the boosted model. On the other hand, the model is
#still not doing well at predicting defaults, predicting only 16/33 = 48% correctly.
#The lack of an even greater improvement may be a function of our relatively small
#training dataset, or it may just be a very difficult problem to solve.

#making mistakes more costlier than others
matrix_dimensions <- list(c('no','yes'),c('no','yes'))
names(matrix_dimensions) <- c('predicted','actual')
matrix_dimensions

error_cost <- matrix(c(0,1,4,0),nrow=2,dimnames = matrix_dimensions)
error_cost

####BUG ! 不知道為啥c50 code called exit with value 1
credit_cost <- C5.0(credit_train[-21],factor(credit_train$default) , costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default , credit_cost_pred , prop.chisq = FALSE , 
           prop.c = FALSE , prop.r =FALSE , dnn = c('actual default','predicted dafault'))
##if 成功，雖提高FP機率，但降低FN機率


