#black box method - neural network
library(neuralnet) # neural networks 
library(h2o) # NN

#exploring data
concrete <- read.csv('concrete.csv')
str(concrete)

#NN work best when the input data are scaled to a narrow range around zero (normalization)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#our normalize() function can be applied to every column
#in the concrete data frame using the lapply() function as follows:
concrete_norm <- as.data.frame(lapply(concrete , normalize))
summary(concrete_norm$strength) #original : summary(concrete$strength)
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1013,]

#training a model
concrete_model <- neuralnet(strength ~ cement+slag + ash + water + 
        superplastic + coarseagg + fineagg + age, data = concrete_train)

plot(concrete_model)

#evaluating model performance
model_results  <- compute(concrete_model , concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength , concrete_test$strength) #0.81
#Correlations close to 1 indicate strong linear relationships between two variables.
#Therefore, the correlation here of about 0.806 indicates a fairly strong relationship.

#improving model performance
concrete_model2 <- neuralnet(strength ~ cement+slag + ash + water + 
        superplastic + coarseagg + fineagg + age, data = concrete_train , hidden = 5)
plot(concrete_model2)

#Notice that the reported error (measured again by SSE) has been reduced from
#5.08 in the previous model to 1.63 here. Additionally, the number of training steps
#rose from 4,882 to 86,849, which should come as no surprise given how much more
#complex the model has become.

model_results2  <- compute(concrete_model2 , concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2 , concrete_test$strength)
#Applying the same steps to compare the predicted values to the true values, we
#now obtain a correlation around 0.92, which is a considerable improvement over
#the previous result of 0.80 with a single hidden node
#----------------------------------------------------------------
h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

train_hex = h2o.uploadFile("concrete.csv", destination_frame = "train_hex")
test_hex = h2o.uploadFile("concrete_test.csv", destination_frame = "test_hex")

h2o_trainmodel <- h2o.deeplearning(x = 1:8 , y= 9 , training_frame = train_hex , 
                          activation = "Tanh", hidden = c(10, 10, 10), epochs = 100)
summary(h2o_trainmodel)

h2o_testmodel <- h2o.predict(h2o_trainmodel , test_hex)
cor(h2o_testmodel,train_hex[,9]) #0.91

#h2o.performance(h2o_testmodel,train_hex) 可能上面model有問題，請參考網路例子重用

