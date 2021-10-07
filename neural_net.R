## set up environment and import data file


rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(keras, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tensorflow, quietly = TRUE)
library(reticulate, quietly = TRUE)


testResponse <- readRDS(file = "testResponse.RDS")
testPred <- readRDS(file = "testPred.RDS")
trainResponse <- readRDS(file = "trainResponse.RDS")
trainPred <- readRDS(file = "trainPred.RDS")

model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

ModelHistory <- model %>% fit(trainPred, trainResponse, epochs = 300, batch_size = 8, validation_split=1/3)



PlotData <- data.frame(x = c(5:ModelHistory$params$epochs), y = ModelHistory$metrics$val_mae [-c(1:4)])
ggplot(PlotData, aes(x = x, y = y)) + geom_smooth() + xlab("Epoch") + ylab("Estimated Validation MAE loss")


model %>% fit(trainPred, trainResponse, epochs = 90, batch_size = 8, validation_data = list(testPred, testResponse))

results <- model %>% evaluate(testPred, testResponse)

model %>% predict(testPred)

