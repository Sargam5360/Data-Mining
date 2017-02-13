# Data Mining Assignment 6
# By Sargam Shah
# UTA ID: 1001275800
# Iris data set


#Read the data

data <- read.csv("C:\\Users\\Sargam\\Desktop\\sem 3\\CSE 5334\\data\\iris.csv")
print(head(data))

summary(data)
nrow(data)


#Separate the data into training and testing

#80 % sampled for training
smp_size <- floor(0.80 * nrow(data))

set.seed(123)

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

iristrain <- data[train_ind, ]
iristest <- data[-train_ind, ]

head(iristrain)
head(iristest)

nrow(iristrain)
nrow(iristest)


#Neural Network

install.packages("neuralnet")

library(neuralnet)


nnet_iristrain <-iristrain


#Binarize the categorical output
nnet_iristrain <- cbind(nnet_iristrain, 
                          iristrain$Species == 'setosa')
 nnet_iristrain <- cbind(nnet_iristrain,
                          iristrain$Species == 'versicolor')
 nnet_iristrain <- cbind(nnet_iristrain, 
                          iristrain$Species == 'virginica')
 names(nnet_iristrain)[6] <- 'setosa'
 names(nnet_iristrain)[7] <- 'versicolor'
 names(nnet_iristrain)[8] <- 'virginica'
 nn <- neuralnet(setosa+versicolor+virginica ~ 
                    Sepal.Length+Sepal.Width
                  +Petal.Length
                  +Petal.Width,
                  data=nnet_iristrain, 
                  hidden=c(3))
 plot(nn)
 mypredict <- compute(nn, iristest[-5])$net.result
 print(mypredict)
 # Put multiple binary output to categorical output
  maxidx <- function(arr) {
    return(which(arr == max(arr)))
  }
 idx <- apply(mypredict, c(1), maxidx)
 prediction <- c('setosa', 'versicolor', 'virginica')[idx]
 table(prediction, iristest$Species)

 
 # Naive Bayes Classification
 
 install.packages("e1071")
 install.packages("klaR")
 require(klaR)
 library(e1071)
 
 model <- naiveBayes(Species~., data=iristrain)
 prediction <- predict(model, iristest[,-5])
 table(prediction, iristest[,5])
 
 
 
 model$tables$Petal.Length
plot(model, type="1")
plot(0:3, xlim=c(0.5,7), col="red", ylab="density",type="n", xlab="Petal Length",main="Petal length distribution for each species")
curve(dnorm(x, model$tables$Petal.Length[1,1], model$tables$Petal.Length[1,2]), add=TRUE, col="red")
curve(dnorm(x, model$tables$Petal.Length[2,1], model$tables$Petal.Length[2,2]), add=TRUE, col="blue")
curve(dnorm(x, model$tables$Petal.Length[3,1], model$tables$Petal.Length[3,2]), add=TRUE, col ="green") 
legend("topright", c("setosa", "versicolor", "virginica"), col = c("red","blue","green"), lwd=1)

  # support vector machine
  
  library(e1071)
  
  svm.model <- svm(Species ~ ., data=iristrain, type='C-classification', kernel='linear',scale=FALSE)
  
  
  pred_result <- predict(model,iristest)
  
  table(pred_result,iristest$species)
  
  
  plot(svm.model, iristrain, Petal.Width ~ Petal.Length,
       slice = list(Sepal.Width = 3, Sepal.Length = 4),color.palette = terrain.colors)
  