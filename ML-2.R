#Naive Bayes case (chapter3) , ft. text mining
Sys.setlocale("LC_ALL", "English")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw) #type element is a character vector

#change to factor
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#data preparation -cleaning and standardizing text data
library(NLP)
library(tm) # ft.NLP
library(SnowballC) #The SnowballC package provides a wordStem() function
library(RColorBrewer)
library(wordcloud)# ft. RcolorBrewer 
library(e1071)
library(gmodels)

#The first step in processing text data involves creating a corpus, 
#which is a collection of text documents
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print (sms_corpus)

#To receive a summary of specific messages, we can use the
#inspect() function with list operators.
inspect(sms_corpus[1:2])

#To view one message, use the as.character() function on a single list element, 
#noting that the double-bracket notation is required:
as.character(sms_corpus[[1]])

#To view multiple documents, we'll need to use as.character() on several items in
#the sms_corpus object. To do so, we'll use the lapply() function, which is a part of a
#family of R functions that applies a procedure to each element of an R data structure.
#These functions, which include apply() and sapply() among others, are one of the
#key idioms of the R language.
lapply(sms_corpus[1:2],as.character)

#clean data
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers)
#Note that the preceding code did not use the content_transformer()
#function. This is because removeNumbers() is built into tm
#along with several other mapping functions that do not need to be
#wrapped. To see the other built-in transformations, simply type
#getTransformations().

sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation)

#EX : wordStem(c('learn','learned','learning','learns'))
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)

lapply(sms_corpus[1:3],as.character)
lapply(sms_corpus_clean[1:3],as.character)

#Data preparation - splitting text documents into words
#Document Term Matrix (DTM) in which rows indicate documents (SMS messages) 
#and columns indicate terms (words).
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#Data preparation - creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

#These labels are not stored in the DTM, so we would need to pull them 
#from the original sms_raw data frame:
sms_train_lables <-sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5559,]$type

#To confirm that the subsets are representative of the complete set of SMS data, 
#let's compare the proportion of spam in the training and test data frames:
prop.table(table(sms_train_lables))
prop.table(table(sms_test_labels))

#Visualizing text data
wordcloud(sms_corpus_clean,min.freq = 70, random.order = FALSE)

spam <- subset(sms_raw, type =='spam')
ham <- subset(sms_raw, type =='ham')

wordcloud(spam$text,max.words=40, scale=c(3,0.5))
wordcloud(ham$text,max.words=20, scale=c(3,0.5))

#Data preparation - creating indicator features for frequent words
sms_freq_words <- findFreqTerms(sms_dtm_train,5) 
str(sms_freq_words)

#inspect(sms_dtm_freq_train)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

#The Naive Bayes classifier is typically trained on data with categorical features.
#This poses a problem, since the cells in the sparse matrix are numeric and measure
#the number of times a word appears in a message. We need to change this to a
#categorical variable that simply indicates yes or no depending on whether the
#word appears at all.

convert_counts <- function(x){
  x <- ifelse(x>0, 'Yes',"No")    
}
#we'll use MARGIN = 2, since we're interested in the columns 
#(MARGIN = 1 is used for rows)
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2 , convert_counts) #turn num into y, n 
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2 , convert_counts) 

#training model
sms_classifier <-naiveBayes(sms_train,sms_train_lables)

#evaluating model
sms_test_pred <- predict(sms_classifier,sms_test)

#compare the predictions to the true values
CrossTable(sms_test_pred,sms_test_labels,prop.chisq = FALSE,
           prop.t = FALSE, dnn= c('predicted','actual'))

#total of only 6 + 30 = 36 of the 1,390 SMS messages were incorrectly classified 
#(2.6 percent). Among the errors were 6 out of 1,207 ham messages 
#that were misidentified as spam, and 30 of the 183 spam
#messages were incorrectly labeled as ham.

#improving model performance
sms_classifier2 <- naiveBayes(sms_train,sms_train_lables,laplace = 1)
sms_test_pred2 <- predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_test_labels,prop.chisq = FALSE,
           prop.t = FALSE, dnn=c('predicted','actual'))

#Adding the Laplace estimator reduced the number of false positives (ham messages
#erroneously classified as spam) from 9 to 7 and the number of false negatives
#from 20 to 28. (其實變更爛了，跟課本不一樣)
