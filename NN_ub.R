## Set up Environment and Import Data
rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR() # restart r session to disocnect any old python attachments

library(keras, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tensorflow, quietly = TRUE)
library(reticulate, quietly = TRUE)
library(caret, quietly = TRUE)

## Set env variables and check Py
# Sys.setenv(RETICULATE_PYTHON = "/home/sean/anaconda3/envs/r-reticulate/bin/python") # for reticulate to call correct python environemnt Google Cloud
Sys.setenv(RETICULATE_PYTHON = "/home/veering/anaconda3/envs/r-reticulate/bin/python") # for reticulate to call correct python environemnt mbp
Sys.getenv("RETICULATE_PYTHON") # check Sys variable is correct
py_config() # check python config

## Set seed for keras
set_random_seed(123)

data <- readRDS(file = "data.RDS") # import model data prerpared earlier and daved in RDS file
plotData_test <- as.data.frame(data[5]) # test reponses and dates
plotData_train <- as.data.frame(data[6]) # train response and dates
testPredictors <- as.matrix(as.data.frame(data[7])) # test predictors, matrix for tf
testResponse <- as.numeric(unlist(data[8])) # test response, numeric vec for tf
trainPredictors <- as.matrix(as.data.frame(data[9])) # training predictors, as matrix for tf
trainResponse <- as.numeric(unlist(data[10])) # training response, as matrix for tf

## check dimensions and type of all data
dim(plotData_test)
typeof(plotData_test)

dim(testPredictors)
typeof(testPredictors)

length(testResponse)
typeof(testResponse)

dim(trainPredictors)
typeof(trainPredictors)

length(trainResponse)
typeof(trainResponse)

## Define function to create sequential keras model with specified hidden layers and units.

build_model <- function(l, u, is, opt, loss, met){ # function recieves # layers, # units, input shape, optimiser, loss and metrics
  ## define building blocks
  inputLayer <- layer_dense(units = u, activation = "relu", input_shape = c(is)) # define input layer with # features (columns) as the single dimension
  outputLayer <- layer_dense(units = 1) # define output layer with no activation function and a single output, sutiable for regression
  hiddenLayer <- list( # define hidden layers as list of 16 dense layers with units u and activation "relu"
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu"),
    layer_dense(unit = u, activation = "relu")
  )
  
  ## build model
  model <- keras_model_sequential( # use keras_seqential to compile model
    name = paste("model_units-",u,"_layer-",l, sep = ""), # give model a meaningful name
    layers = c(inputLayer,  # conc input layer and output layer with user defined number of hidden layers from "hiddenLayer"
               hiddenLayer[1:l],
               outputLayer)
  )
  
  ## Compile Model
  model %>% compile(
    optimizer = opt,
    loss = loss,
    metrics = c(met)
  )
}

test_tune_grid <- function(trainPredictors, trainResponse, testPredictors, testResponse, k, layers, units, batch, e, delta, patience){
  ## Create 4 x splits set from training data for validation
  
  set.seed(123) # set seed
  trainIndex <- createDataPartition(trainResponse, p = 0.8, times = k, list = FALSE) # use caret to create 4 stratified train / validation sets
  
  ## Build tuning grid
  tuneGrid <- expand.grid(
    l = layers, # layers
    u = units, # units
    b = batch # obs in batch
  )
  
  ## build results grid to store MSE and MAE from training and validation
  resultsGrid <- expand.grid(
    k = 1:k, # the for loops will iterate over k first
    l = layers, # layers
    u = units, # units
    b = batch, # obs in batch
    tg = NA, # NA to hold place for metrics
    i = NA,
    mae = NA, 
    mae_val = NA,
    num_epoch = NA
  )
  
  ## Call tensor board and define call backs
  tensorboard("my_log_dir")
  callbacks = list(
    callback_tensorboard( # call TB
      log_dir = "my_log_dir",
      histogram_freq = 1),
    callback_early_stopping( # call early stopping
      monitor = "val_loss", # use loss on validation set as metric
      min_delta = delta, # min change to metric
      patience = patience) # epochs to tolerate < min change before stopping
  )
  
  ## Build and train each model over all 4 folds
  
  rg <- 1 # results grid counter
  
  ## Train models and record results
  for(tg in (1:nrow(tuneGrid))) { # iterate over each row of the tune grid
    for(i in 1:k){ # iterate over each fold
      cat("fold # ", i," from tg # ", tg, " storing results in rg # ", rg, "\n") # tracker for console
      
      ## make training and Validation split 
      x <- as.matrix(trainPredictors[trainIndex[, i], ]) # predictors as x
      y <- as.numeric(trainResponse[trainIndex[, i]]) # response as y
      x_val <- as.matrix(trainPredictors[-trainIndex[, i], ]) # validation predictors
      y_val <- as.numeric(trainResponse[-trainIndex[, i]]) # validation response
      
      #####
      ### Troubleshooting
      # dim(x)
      # typeof(x)
      # length(y)
      # typeof(y)
      # dim(x_val)
      # typeof(x_val)
      # length(y_val)
      # typeof(y_val)
      
      ## use make function to define and compile model 
      set.seed(123) # set seed
      model <- build_model(tuneGrid[tg, 1], # build model this # layers from tg
                           tuneGrid[tg, 2],  # build model with # units from tg
                           ncol(trainPredictors), # input shape 
                           "adam", # optimiser
                           "mse", # loss function
                           "mae" # metrics
      ) 
      
      ## Fit model
      history <- model %>% fit( # fit model and record results in history
        x, # training predictors
        y, # training response
        epochs = e, # epochs (predefined for easy adjustment)
        batch_size = tuneGrid[tg, 3], # batch size from tuning grid 
        validation_data = list( # list validation predictors and response
          x_val,
          y_val),
        callbacks = callbacks # predefined call backs
      )
      
      ## return metrics of interest to resultsGrid
      resultsGrid$tg[rg] <- tg # store tg number for result traceability
      resultsGrid$i[rg] <- i # return fold number for result traceability
      resultsGrid$mae[rg] <- min(history$metrics$mae) # assume the min value is the final value (ok for selecting HP with call back early stopping)
      resultsGrid$mae_val[rg] <- min(history$metrics$val_mae)
      resultsGrid$num_epoch[rg] <- length(history$metrics$mae) # count epoch's by length of metric vector as it stops when call back stops training
      
      rg <- rg + 1 # increase results grid counter by 1
    }
  }
  
  return(resultsGrid)
}

## Corse Tune

## define paramaters for corse tuning grid
k <- 4 # define number of folds
layers <- c(2,4,8,16) # define # layers to test
units <- c(4,8,16,20) # define # units to test
batch <- c(4,8,16,20) # batch size to trial
e <- 300 # define number of epochs
delta <- .001
patience <- 5

## record start time
start <- Sys.time()

resultsGrid <- test_tune_grid(trainPredictors, trainResponse, testPredictors, testResponse, k, layers, units, batch, e, delta, patience)

## record finish time
finishTime <- Sys.time()

## save resultsGrid to RDS for use later
saveRDS(resultsGrid,"resultsGrid_NN_corse_ubuntu.RDS")

## Fine Tune
## define paramaters for fine tuning grid
k <- 4 # define number of folds
layers <- c(6,7,8,9,10,11,12) # define # layers to test
units <- c(c(16,17,18,19,20)) # define # units to test
batch <- c(1, 2, 4) # batch size to trial
e <- 500 # define number of epochs
delta <- .000005
patience <- 10

## record start time
start <- Sys.time()

resultsGrid <- test_tune_grid(trainPredictors, trainResponse, testPredictors, testResponse, k, layers, units, batch, e, delta, patience)

## record finish time
finishTime <- Sys.time()

## save resultsGrid to RDS for use later
saveRDS(resultsGrid,"resultsGrid_NN_fine_gcloud.RDS")

