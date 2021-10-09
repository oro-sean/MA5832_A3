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

data <- readRDS(file = "data.RDS") # import data from rds file
results <- as.data.frame(data[5]) # unlist each component and change to data frame, matrix and vector as appropriate
trainAss <- as.data.frame(data[6])
testPred <- as.matrix(as.data.frame(data[7]))
testResponse <- unlist(data[8])
trainPred <- as.matrix(as.data.frame(data[9]))
trainResponse <- unlist(data[10])



MARS_TUNE <- train(x = trainPred, 
                   y = trainResponse,
                   method = "earth",
                   metric = "MAE",
                   trControl = trainControl( method = "cv",
                                             number = 10),
                   tuneGrid = expand.grid(degree = 1:3,
                                          nprune = 10:25)
                   )

MARS_TUNE$results
ggplot(MARS_TUNE)
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
