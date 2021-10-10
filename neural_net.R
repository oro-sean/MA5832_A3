## set up environment and import data file

rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(keras, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tensorflow, quietly = TRUE)
library(reticulate, quietly = TRUE)
library(caret, quietly = TRUE)

Sys.setenv(RETICULATE_PYTHON = "/home/veering/anaconda3/envs/r-reticulate/bin/python")
Sys.getenv("RETICULATE_PYTHON")
py_config()
start <- Sys.time()
data <- readRDS(file = "data.RDS")
results <- as.data.frame(data[5])
trainAss <- as.data.frame(data[6])
testPred <- as.matrix(as.data.frame(data[7]))
testResponse <- as.numeric(unlist(data[8]))
trainPred <- as.matrix(as.data.frame(data[9]))
trainResponse <- as.numeric(unlist(data[10]))

trainIndex <- createDataPartition(trainResponse, p = 0.8, times = 4, list = FALSE)

model_2_4 <- keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(units = 1)

model_2_8 <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(units = 1)

model_2_16 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(units = 1)

model_2_32 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(units = 1)

model_4_4 <- keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(units = 1)

model_4_8 <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(units = 1)

model_4_16 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(units = 1)

model_4_32 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(units = 1)

model_8_4 <- keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(units = 1)

model_8_8 <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(units = 1)

model_8_16 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(units = 1)

model_8_32 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(units = 1)

model_16_4 <- keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(unit = 4, activation = "relu") %>%
  layer_dense(units = 1)

model_16_8 <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(unit = 8, activation = "relu") %>%
  layer_dense(units = 1)

model_16_16 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(unit = 16, activation = "relu") %>%
  layer_dense(units = 1)

model_16_32 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(ncol(testPred))) %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(unit = 32, activation = "relu") %>%
  layer_dense(units = 1)

model_2_4 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_2_8 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_2_16 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_2_32 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_4_4 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_4_8 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_4_16 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_4_32 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_8_4 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_8_8 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_8_16 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_8_32 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_16_4 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_16_8 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_16_16 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

model_16_32 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)


tensorboard("my_log_dir")
callbacks = list(callback_tensorboard(log_dir = "my_log_dir",
                                      histogram_freq = 1,
                                      ),
                 callback_early_stopping(
                   monitor = "mae", patience = 5,
                   min_delta = .005,
                   mode = "min"
  )
)


his <- c()
for(b in c(2,4,8,16,32,64)){
for( i in 1:4){
ModelHistory <- model_2_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_2_8 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_2_16 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_2_32 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_4_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_4_8 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_4_16 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_4_32 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_8_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_8_8 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_8_16 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_8_32 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_16_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_16_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_16_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

ModelHistory <- model_16_4 %>% fit(trainPred[trainIndex[ ,i], ], trainResponse[trainIndex[, i]], epochs = 100, batch_size = b, validation_data = list(trainPred[-trainIndex[, i], ], trainResponse[-trainIndex[ ,i]]), callbacks = callbacks)
his <- rbind(his, ModelHistory$metrics) 

}
}
finishtime <- Sys.time()
PlotData <- data.frame(x = c(5:ModelHistory$params$epochs), y = ModelHistory$metrics$val_mae [-c(1:4)])
ggplot(PlotData, aes(x = x, y = y)) + geom_smooth() + xlab("Epoch") + ylab("Estimated Validation MAE loss")


model %>% fit(trainPred, trainResponse, epochs = 10, batch_size = 8, validation_data = list(testPred, testResponse))

results <- model %>% evaluate(testPred, testResponse)

results$Predictions <- model %>% predict(testPred) %>% as.numeric()
trainAss$Predictions <- model %>% predict(trainPred) %>% as.numeric()

results <- as.data.frame(results)
ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 
ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Y), colour = "blue") 
 