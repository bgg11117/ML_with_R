#clustering (kmeans)
library(stats) #kmeans

#exploring data
teens <- read.csv('snsdata.csv')
str(teens) #gender中有很多NA，會是個問題
table(teens$gender) #忽略NA
table(teens$gender,useNA = 'ifany')
summary(teens$age) #有極端值
teens$age <- ifelse(teens$age >=13 & teens$age<20 , teens$age , NA)
summary(teens$age)

#data preparation - dummy coding missing values
teens$female <- ifelse(teens$gender == 'F' & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

table(teens$gender,useNA = 'ifany')
table(teens$female, useNA = 'ifany')
table(teens$no_gender, useNA = "ifany")

#data preparation - imputing the missing values
mean(teens$age) #因為有NA值
mean(teens$age , na.rm=TRUE)
aggregate(data=teens , age~gradyear , mean , na.rm= TRUE)

ave_age <- ave(teens$age , teens$gradyear , FUN =function(x) mean(x,na.rm=TRUE))
teens$age <- ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

#training a model 
interests <- teens[5:40]
# scale
interests_z <- data.frame(lapply(interests,scale))

set.seed(2345)
teen_clusters <- kmeans(interests_z,5)

#evaluating model performance
teen_clusters$size

teen_clusters$centers
#The rows of the output (labeled 1 to 5) refer to the five clusters, while the numbers
#across each row indicate the cluster's average value for the interest listed at the top
#of the column.

#improving model performance
teens$cluster <- teen_clusters$cluster
teens[1:5 ,c('cluster','gender','age','friends')]

aggregate(data = teens , age~cluster , mean)
aggregate(data = teens , female~cluster , mean)

aggregate(data = teens , friends~cluster , mean)
