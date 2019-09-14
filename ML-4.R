#linear regression 
launch <- read.csv("challenger.csv")
library(psych) #enhanced scatterplot matrix

#y=a+bx
b <- cov(launch$temperature, launch$distress_ct )/ var(launch$temperature)
a <- mean(launch$distress_ct) - b * mean(launch$temperature)

#r<- cov(launch$temperature, launch$distress_ct) /
  #(sd(launch$temperature) * sd(launch$distress_ct))

r<- cor(launch$temperature,launch$distress_ct)

reg<- function(y,x){
  x <- as.matrix(x)
  x <- cbind(Intercerpt = 1,x)
  b <- solve(t(x) %*% x)%*% t(x) %*% y
  colnames(b) <- 'estimate'
  print (b)
}

str(launch)
reg(y = launch$distress_ct , x =launch[3]) #launch$temperature
reg(y = launch$distress_ct , x =launch[3:5])

#collecting data
insurance <- read.csv('insurance.csv',stringsAsFactors = FALSE)
str(insurance)
summary(insurance$charges) #dependent variables
hist(insurance$charges) #right-skewed distribution
table(insurance$region)

#exploring features -correlation matrix
cor(insurance[c('age','bmi','children','charges')])

#visualizing features - scatterplot matrix
pairs(insurance[c('age','bmi','children','charges')])
pairs.panels(insurance[c('age','bmi','children','charges')])

#training a model 
ins_model <- lm(charges~ . ,data = insurance)
ins_model #(種類的會有其一被抓出來當dummy variable)
#When adding a dummy variable to a regression model, one category is always left
#out to serve as the reference category. The estimates are then interpreted relative to
#the reference. In our model, R automatically held out the sexfemale, smokerno, and
#regionnortheast variables, making female non-smokers in the northeast region the
#reference group.

#Thus, males have $131.40 less medical expenses each year relative to
#females and smokers cost an average of $23,847.50 more than non-smokers per year.
#The coefficient for each of the three regions in the model is negative, which implies that
#the reference group, the northeast region, tends to have the highest average expenses.

#By default, R uses the first level of the factor variable as the reference.
#If you would prefer to use another level, the relevel() function can
#be used to specify the reference group manually.

#evaluating model performance
summary(ins_model)
#Small p-values suggest that the true coefficient is
#very unlikely to be zero, which means that the feature is extremely unlikely
#to have no relationship with the dependent variable.

#improving model performance
insurance$age2 <- insurance$age^2

#transformation - convert numeric variable to a binary
insurance$bmi30 <- ifelse(insurance$bmi>=30 ,1 ,0)

#model specification - adding interaction effects
#To have the obesity indicator (bmi30) and the smoking indicator (smoker) interact,
#we would write a formula in the form expenses ~ bmi30*smoker.
#The * operator is shorthand that instructs R to 
#model expenses ~ bmi30 + smokeryes + bmi30:smokeryes.
#The : (colon) operator in the expanded form indicates that 
#bmi30:smokeryes is the interaction between the two variables.

#putting together - an improved regression model
ins_model2 <- lm(charges~ age +age2 +children+ bmi + sex + bmi30*smoker + region ,
                 data = insurance)
summary(ins_model2)
#R-squared 0.75→0.87
#The interaction between obesity and smoking suggests a massive
#effect; in addition to the increased costs of over $13,404 for smoking alone, obese
#smokers spend another $19,810 per year. This may suggest that smoking exacerbates
#diseases associated with obesity.
