#How To Estimate Model Accuracy in R Using The Caret Package

#Estimating Model Accuracy



# load the libraries
library(caret)
library(klaR)
## load the iris dataset
data(iris)



#Data Split
## define an 80%/20% train/test split of the dataset
trainIndex <- createDataPartition(iris$Species, p=0.80, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
## train a naive bayes model
model_split <- NaiveBayes(Species~., data=data_train)
## make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model_split, x_test)
## summarize results
confusionMatrix(predictions$class, y_test)


#Bootstrap
## define training control
train_control_bt <- trainControl(method="boot", number=100)
## train the model
model_bt <- train(Species~., data=iris, trControl=train_control_bt, method="nb")
## summarize results
print(model_bt)


#k-fold Cross Validation
## define training control
train_control_kfcv <- trainControl(method="cv", number=10)
## fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
## train the model
model_kfcv <- train(Species~., data=iris, trControl=train_control_kfcv, method="nb", tuneGrid=grid)
## summarize results
print(model_kfcv)


#Repeated k-fold Cross Validation
## define training control
train_control_RKFCV <- trainControl(method="repeatedcv", number=10, repeats=3)
## train the model
model_RKFCV <- train(Species~., data=iris, trControl=train_control_RKFCV, method="nb")
## summarize results
print(model_RKFCV)


#Leave One Out Cross Validation
## define training control
train_control_LOOCV <- trainControl(method="LOOCV")
## train the model
model_LOOCV <- train(Species~., data=iris, trControl=train_control_LOOCV, method="nb")
## summarize results
print(model_LOOCV)
