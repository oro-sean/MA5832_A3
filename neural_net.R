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
testPred <- as.matrix(as.data.frame(data[7]))
testResponse <- unlist(data[8])
trainPred <- as.matrix(as.data.frame(data[9]))
trainResponse <- unlist(data[10])

conda_binary(conda = "auto")
conda_python(conda = "auto")
conda_list(conda = "auto")

install_keras(method = "conda", conda = "auto", envname = "r-reticulate")
conda_install("r-reticulate", "tensorflow")

Sys.getenv("RETICULATE_PYTHON")
Sys.setenv(RETICULATE_PYTHON = "C:/Users/veering_windows/anaconda3/envs/r-reticulate/python.exe")

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


model %>% fit(trainPred, trainResponse, epochs = 125, batch_size = 8, validation_data = list(testPred, testResponse))

results <- model %>% evaluate(testPred, testResponse)

results$Predictions <- model %>% predict(testPred)
trainAss$Predictions <- model %>% predict(trainPred)

ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 
ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 
