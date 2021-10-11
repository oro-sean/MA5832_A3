# Set up environment and import Data

rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(ggplot2, quietly = TRUE)
library(earth, quietly = TRUE)
library(caret, quietly = TRUE)
library(vip, quietly = TRUE)
library(pdp, quietly = TRUE)
library(data.table, quietly = TRUE)

data <- readRDS(file = "data.RDS") # import model data prerpared earlier and daved in RDS file
plotData_test <- as.data.frame(data[5]) # test reponses and dates
plotData_train <- as.data.frame(data[6]) # train response and dates
testPredictors <- as.matrix(as.data.frame(data[7])) # test predictors, matrix for tf
testResponse <- as.numeric(unlist(data[8])) # test response, numeric vec for tf
trainPredictors <- as.matrix(as.data.frame(data[9])) # training predictors, as matrix for tf
trainResponse <- as.numeric(unlist(data[10])) # training response, as matrix for tf

## First perform a course grid search to assess the accuracy measure and rough hyper parameters
set.seed(123) # set seed
MARS_TUNE <- train(x = trainPredictors, # use caret train to train the model on training predictors
                   y = trainResponse, # training response
                   method = "earth", # use the MARS algorithm
                   metric = "MAE", # initially assess the model using Mean Absolute Error
                   trControl = trainControl( method = "cv", # perform 10 fold cross validation
                                             number = 10),
                   tuneGrid = expand.grid(degree = 1:4, # tune for degree 1 --> 4
                                          nprune = seq(5,45,5)) # tune over the range 4 --> 45
                   )

## Plot results

ggplot(MARS_TUNE) + # quick plot of MAE vs prune and degree
  labs(title = "Accuracy vs # Terms (prune) by Degree") + # add tittle
  theme_classic()

accPlot_data <- as.data.frame(MARS_TUNE$resample) # return accuracy for all folds of the best tune
accPlot_data <- melt(accPlot_data, id.vars = "Resample") # melt data for plotting
accPlot_data$fold <- 1:10 # add fold variable so numeric value for fold

accPlot <- ggplot(data = accPlot_data) +
  geom_point(aes(x = fold, y = value, color = variable)) + # plot fold on x axis, value on y axis, colour by metric
  geom_line(aes(x = fold, y = value, color = variable))
accPlot

## Refined Model Tune
set.seed(123)
MARS_REFINED <- train(x = trainPredictors, # use caret train to train the model on training predictors
                   y = trainResponse, # training response
                   method = "earth", # use the MARS algorithm
                   metric = "Rsquared", # train using Rsquared
                   trControl = trainControl( method = "LOOCV", # use leave one out cross validation
                                             number = 10),
                   tuneGrid = expand.grid(degree = 1:2, # tune for degree 1 --> 2
                                          nprune = 15:25) # tune over the range 15 --> 25
)


MARS_TUNE$results$degree
MOD <- earth(x = trainPred, y = trainResponse, pmethod = "cv", ncross = 3, nfold = 5)

trainAss$Predictions_RMSE <- predict(MARS_TUNE$finalModel)
results$Predictions <- predict(MARS_TUNE$finalModel, newdata = testPred)
plot(MOD)

ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions_R), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") +geom_line(aes(x = Date, y = Predictions_RMSE), colour = "green") 
ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 

MARS_TUNE$finalModel$selected.terms

MARS_TUNE$resample

vip(MARS_TUNE$finalModel, num_features = 20, geom = "point")

partial(MARS_TUNE, pred.var = "T5_mean_Window_19_lagged_0") %>% autoplot()
summary(MARS_TUNE$finalModel)

MARS_TUNE$finalModel %>% coef()

ggplot(data = MARS_TUNE$resample) + geom_line(aes)
