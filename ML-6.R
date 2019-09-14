#Market basket analysis(Apriori)
#In order to create the sparse matrix data structure from the transactional data, 
#we can use the functionality provided by the arules package.
library(arules) #read in sparse matrix

#data preparation - creating a sparse matrix for transaction data
groceries <- read.transactions('groceries.csv' , sep = ',')
summary(groceries)
#The density value of 0.02609146 (2.6 percent) refers to the proportion of nonzero
#matrix cells. Since there are 9,835 * 169 = 1,662,115 positions in the matrix, we can
#calculate that a total of 1,662,115 * 0.02609146 = 43,367 items were purchased during
#the store's 30 days of operation (ignoring the fact that duplicates of the same items
#might have been purchased). With an additional step, we can determine that the
#average transaction contained 43,367 / 8,835 = 4.409 distinct grocery items. 
inspect(groceries[1:5]) #first five transactions
itemFrequency(groceries[,1:3]) #This allows us, for instance, to view the support
#level for the first three items in the grocery data

#visualizing item support-item frequency plots
itemFrequencyPlot(groceries,support = 0.1)
itemFrequencyPlot(groceries,topN = 20)

#visualizing the transaction data - plotting the sparse matrix
image(groceries[1:5])
image(sample(groceries,100)) #發現有一些col很密集，表熱門商品

#training a model on the data
apriori(groceries)
#if we attempt to use the default settings of support = 0.1 and
#confidence = 0.8, we will end up with a set of zero rules
groceryrules <- apriori(groceries , parameter = list(support = 0.006 , 
                        confidence = 0.25, minlen = 2))
#One way to approach the problem of setting a minimum support threshold is to
#think about the smallest number of transactions you would need before you would
#consider a pattern interesting. For instance, you could argue that if an item is
#purchased twice a day (about 60 times in a month of data), it may be an interesting
#pattern. From there, it is possible to calculate the support level needed to find only
#the rules matching at least that many transactions. Since 60 out of 9,835 equals 0.006

groceryrules 

#evaluating model performance
summary(groceryrules)
inspect(groceryrules[1:3])

#improving model performance
inspect(sort(groceryrules,by='lift')[1:5])

#taking subsets of association rules
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

#saving association rules to a file
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)