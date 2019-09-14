#black box method , case OCR - SVM 
library(e1071)
library(klaR) #ft.library(MASS)
library(MASS)
library(kernlab)

#exploring data
letters <- read.csv('letterdata.csv')
str(letters)
#Recall that SVM learners require all features to be numeric, and moreover, that
#each feature is scaled to a fairly small interval. In this case, every feature is an
#integer, so we do not need to convert any factors into numbers. On the other hand,
#some of the ranges for these integer variables appear fairly wide. This indicates that
#we need to normalize or standardize the data. However, we can skip this step for
#now, because the R package that we will use for fitting the SVM model will perform
#the rescaling automatically.

#already randmoized the data
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

#training a model
letter_classifier <- ksvm(letter ~. , data = letters_train , kernel = 'vanilladot')
letter_classifier

#evaluating model performance
letter_predictions <- predict(letter_classifier , letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)
#The value of 5 in row B and column D indicates
#that there were five cases where the letter D was misidentified as a B.

agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement)) #accuracy 84%

#improving model performance
letter_classifier_rbf <- ksvm(letter ~., data = letters_train , kernel = 'rbfdot')
letter_predictions_rbf <- predict(letter_classifier_rbf , letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))#accuracy 93%

#By simply changing the kernel function, we were able to increase the accuracy
#of our character recognition model from 84 percent to 93 percent.

#-------libsvm
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

tuned<- tune.svm(letter ~. , data = letters_train, gamma = 10^(-3:-1), cost = 10^(-1:1))
summary(tuned)
#- best parameters: gamma 0.1 cost10
# best performance: 0.0269375 

#evaluating model performance
letter_svm_model <- svm(letter ~. , data = letters_train, gamma = 0.1, cost =10 ) 
letter_svm_pred = predict(letter_svm_model, letters_test)

table(letter_svm_pred, letters_test$letter)

agreement_svm <- letter_svm_pred == letters_test$letter
table(agreement_svm)
prop.table(table(agreement_svm)) #accuracy 97.4%

#improving model performance

