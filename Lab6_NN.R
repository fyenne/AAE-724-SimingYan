# Lab 6: Neural network
# AAE 722, Summer 2020
# Jiarui Zhang (Based on Gaurav Doshi, 2019)
# 07/27/2020
#################################################

rm(list=ls())

# Read the Data

setwd("/Users/jiaruizhang/Documents/AAE722/6. Neural Network")
data = read.csv("cereals.csv", header=T)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(nrow(data),size = samplesize)

# Create training and test set
datatrain = data[index, ]
datatest = data[-index, ]

## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
# formula -- (xi - center)/scale

## Fit neural network 

# install library
# install.packages("neuralnet")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
?neuralnet
# err.fct -- default -- sse
# act.fct -- default -- logistic activation function, can define it by yourself
# linear.output = T -- regression or classification

NN = neuralnet(rating ~ ., trainNN, hidden = 3 ,algorithm = "backprop",  learningrate = 0.001, linear.output = T )
# plot neural network
plot(NN)

NN = neuralnet(rating ~ ., trainNN, hidden = 3 , linear.output = T ) # resilient backpropagation
plot(NN)
# 1) Rprop doesn't use the magnitude of the gradient to determine a weight delta; instead, it uses only the sign of the gradient. 
# 2) Instead of using a single learning rate for all weights and biases, Rprop maintains separate weight deltas for each weight and bias, 
# and adapts these deltas during training.

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)
# scale

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN

## Cross validation of neural network model (built using different number of training samples)

# install relevant libraries
# install.packages("boot")
# install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)

# Why different results? -- ramdom weights assgined at the begining.

## Prepare boxplot
boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")

## Variation of median RMSE 
# install.packages("matrixStats")
library(matrixStats)

med = colMedians(Matrix.RMSE)

X = seq(10,65)

plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
