# Set up environment and import Data
rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(ggplot2, quietly = TRUE)
library(earth, quietly = TRUE)

data <- readRDS(file = "data.RDS")
results <- as.data.frame(data[5])
trainAss <- as.data.frame(data[6])
testPred <- as.data.frame(data[7])
testResponse <- as.data.frame(data[8])
trainPred <- as.data.frame(data[9])
trainResponse <- as.data.frame(data[10])

MOD <- earth(x = trainPred, y = trainResponse, pmethod = "cv", ncross = 3, nfold = 5)
summary(MOD)
results$Predictions <- predict(MOD, newdata = testPred)
trainAss$Predictions <- predict(MOD)
plot(MOD)

ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 
ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 