#Decision Tree(1R , ripper)
library(RWeka)

#Since all the 22 features and the target class are nominal, 
#we will set stringsAsFactors = TRUE and take advantage of the automatic factor conversion
mushrooms <- read.csv('mushrooms.csv',stringsAsFactors = TRUE)
str(mushrooms)
mushrooms$veil_type <- NULL
#It is likely that this data element was somehow coded incorrectly.因為只有1 level
table(mushrooms$type) #e = edible , p = poisonous

#For the purposes of this experiment, we will consider the 8,214 samples in the
#mushroom data to be an exhaustive set of all the possible wild mushrooms. This
#is an important assumption, because it means that we do not need to hold some
#samples out of the training data for testing purposes.

#training a model (1R)
mushroom_1R <- OneR(type~. , data = mushrooms)
mushroom_1R #the odor feature was selected for rule generation.

#evaluating a model
summary(mushroom_1R)
#Table columns indicate the predicted class of the mushroom
#while the table rows separate the 4,208 edible mushrooms from the 3,916 poisonous
#mushrooms. Examining the table, we can see that although the 1R classifier did
#not classify any edible mushrooms as poisonous, it did classify 120 poisonous
#mushrooms as edible—which makes for an incredibly dangerous mistake!

#improving model performance (Ripper)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
#First, the algorithm found a large
#group of poisonous mushrooms uniquely distinguished by their foul odor. Next, it
#found smaller and more specific groups of poisonous mushrooms. By identifying
#covering rules for each of the varieties of poisonous mushrooms, all of the remaining
#mushrooms were found to be edible.
