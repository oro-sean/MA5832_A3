## set up environment and import data file

rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(keras, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tensorflow, quietly = TRUE)
library(reticulate, quietly = TRUE)

data <- readRDS(file = "data.RDS")
results <- as.data.frame(data[5])
trainAss <- as.data.frame(data[6])
testPred <- as.data.frame(data[7])
testResponse <- as.data.frame(data[8])
trainPred <- as.data.frame(data[9])
trainResponse <- as.data.frame(data[10])


model <- keras_model_sequential() %>%
  layer_dense(units = 19, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 19, activation = "relu") %>%
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

