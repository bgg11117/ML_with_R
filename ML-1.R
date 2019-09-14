#KNN case (chapter2)
library(class)
library(gmodels)

wbcd <- read.csv('wisc_bc_data.csv',stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[,-1] #����id

table(wbcd$diagnosis)
typeof(wbcd$diagnosis) #charactor
#tagret feature need to be factor

wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c('B','M'),
                         labels = c('Benign','Malignant')) #chang B,M to benign,malignant

#���p�ƫ�@��
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
#���h�Ӷ��[C()
summary(wbcd[c('radius_mean','area_mean','smoothness_mean')])
#�t�Z�L�j�A����normalize(��max-min normalization)

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
} #EX :normalize(c(1,2,3,4,5))

#����normalize
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize)) #�]���Ĥ@��Odiagnosis
summary(wbcd_n$area_mean)

#Data preparation
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[-c(1:469),] #or wbcd_n[470:569,]

#For training the k-NN model, we will need to store these class labels in factor vectors,
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <-wbcd[470:569,1]

#training model
wbcd_test_pred <- knn(train = wbcd_train , test = wbcd_test ,
                      cl = wbcd_train_labels , k=21)

#evaluate model performance (�Hmalignant���D)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE) #���������n���F��
#TN = 77/100 (top left) , TP = 21/100 (bottom right)
#FN = 2/100 (lower left) , FP = (top right) , #FP error <�@FN 

#improving model performance 
#��z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))#�����Ĥ@��
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test ,
                      cl = wbcd_train_labels , k =21)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)

#testing alternative values of K