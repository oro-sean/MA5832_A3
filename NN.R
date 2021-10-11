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
Sys.setenv(RETICULATE_PYTHON = "/home/sean/anaconda3/envs/r-reticulate/bin/python") # for reticulate to call correct python environemnt Google Cloud
#Sys.setenv(RETICULATE_PYTHON = "/home/veering/anaconda3/envs/r-reticulate/bin/python") # for reticulate to call correct python environemnt mbp
Sys.getenv("RETICULATE_PYTHON") # check Sys variable is correct
py_config() # check python config

## Set seed for keras
set_random_seed(123)

## Read data in
#####
data <- readRDS(file = "data.RDS") # import model data prerpared earlier and daved in RDS file
plotData_test <- as.data.frame(data[5]) # test reponses and dates
plotData_train <- as.data.frame(data[6]) # train response and dates
testPredictors <- as.matrix(as.data.frame(data[7])) # test predictors, matrix for tf
testResponse <- as.numeric(unlist(data[8])) # test response, numeric vec for tf
trainPredictors <- as.matrix(as.data.frame(data[9])) # training predictors, as matrix for tf
trainResponse <- as.numeric(unlist(data[10])) # training response, as matrix for tf
#####

## check dimensions and type of all data
#####
dim(testPredictors)
typeof(testPredictors)

length(testResponse)
typeof(testResponse)

dim(trainPredictors)
typeof(trainPredictors)

length(trainResponse)
typeof(trainResponse)

## Define function to create sequential keras model with specified hidden layers and units.
#####
build_model <- function(l, u, is, opt, loss, met, dr){ # function recieves # layers, # units, input shape, optimiser, loss and metrics
  ## define building blocks
  inputLayer <- layer_dense(units = u, activation = "relu", input_shape = c(is)) # define input layer with # features (columns) as the single dimension
  outputLayer <- layer_dense(units = 1) # define output layer with no activation function and a single output, sutiable for regression
  hiddenLayer <- list( # define hidden layers as list of 16 dense layers with units u and activation "relu"
    #####
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
  #####
  
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

build_model_reg <- function(l, u, is, opt, loss, met, dr){ # function recieves # layers, # units, input shape, optimiser, loss and metrics
  
  ## define building blocks
  inputLayer <- layer_dense(units = u, activation = "relu", input_shape = c(is)) # define input layer with # features (columns) as the single dimension
  outputLayer <- layer_dense(units = 1) # define output layer with no activation function and a single output, sutiable for regression
  hiddenLayer <- list( # define hidden layers as list of 16 dense layers with units u and activation "relu"
    #####
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr),
    layer_dense(unit = u, activation = "relu"),
    layer_dropout(rate = dr)
  )
  #####
  
  ## build model
  model <- keras_model_sequential( # use keras_seqential to compile model
    name = paste("model_units-",u,"_layer-",l, "_reg-", dr, sep = ""), # give model a meaningful name
    layers = c(inputLayer,  # conc input layer and output layer with user defined number of hidden layers from "hiddenLayer"
               hiddenLayer[1:(l*2)], # double layer as each hiden layer is now 2 elemenst long
               outputLayer)
  )
  
  ## Compile Model
  model %>% compile(
    optimizer = opt,
    loss = loss,
    metrics = c(met)
  )
}

test_tune_grid <- function(model_builder, trainPredictors, trainResponse, k, layers, units, batch, e, delta, patience, dropout, aim){
  ## Create 4 x splits set from training data for validation
  
  set.seed(123) # set seed
  trainIndex <- createDataPartition(trainResponse, p = 0.8, times = k, list = FALSE) # use caret to create 4 stratified train / validation sets
  
  ## Build tuning grid
  #####
  tuneGrid <- expand.grid(
    l = layers, # layers
    u = units, # units
    b = batch, # obs in batch
    dr = dropout # dropout rate
  )
  #####
  
  ## build results grid to store MSE and MAE from training and validation
  #####
  resultsGrid <- expand.grid(
    k = 1:k, # the for loops will iterate over k first
    l = layers, # layers
    u = units, # units
    b = batch, # obs in batch
    dr = dropout, # dropout rate
    tg = NA, # NA to hold place for metrics
    i = NA,
    mae = NA, 
    mae_val = NA,
    num_epoch = NA
  )
  #####
  
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
  histVal  <- c() # reserve variable name
  histTrain <- c() # reserve variable name
  
  ## Train models and record results
  for(tg in (1:nrow(tuneGrid))) { # iterate over each row of the tune grid
    for(i in 1:k){ # iterate over each fold
      cat("fold # ", i," from tg # ", tg, " storing results in rg # ", rg, "\n", # tracker for console
          "Layer = ", tuneGrid[tg, 1],
          "Unit = ", tuneGrid[tg, 2],
          "Batch Size = ", tuneGrid[tg, 3],
          "Dropout Rate = ", tuneGrid[tg, 4], "\n") 
      
      ## make training and Validation split 
      x <- as.matrix(trainPredictors[trainIndex[, i], ]) # predictors as x
      y <- as.numeric(trainResponse[trainIndex[, i]]) # response as y
      x_val <- as.matrix(trainPredictors[-trainIndex[, i], ]) # validation predictors
      y_val <- as.numeric(trainResponse[-trainIndex[, i]]) # validation response
      
      ### Troubleshooting
      #####
      # dim(x)
      # typeof(x)
      # length(y)
      # typeof(y)
      # dim(x_val)
      # typeof(x_val)
      # length(y_val)
      # typeof(y_val)
      #####
      
      ## use make function to define and compile model 
      set.seed(123) # set seed
      model <- model_builder(l = tuneGrid[tg, 1], # build model this # layers from tg
                             u = tuneGrid[tg, 2],  # build model with # units from tg
                             is = ncol(trainPredictors), # input shape 
                             opt = "adam", # optimiser
                             loss = "mse", # loss function
                             met = "mae", # metrics
                             dr = tuneGrid[tg, 4] # dropout rate
      ) 
      
      ## Fit model
      set.seed(123) # set seed
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
      
      ## return metric for each epoch
      histVal <- rbind(histVal, history$metrics$val_mae) # combine with other folds
      histTrain <- rbind(histTrain, history$metrics$mae) # combine with other folds
      
      rg <- rg + 1 # increase results grid counter by 1
    }
  }
  if(aim == 1)  return(resultsGrid)
  if(aim == 2)  return(list(histVal, histTrain))
}
#####

## Course Tune
#####
## record start time
## record start time
startTime <- Sys.time()

resultsGrid <- test_tune_grid( # call test tuen grid to build and test model with the following parameters
  model_builder = build_model, # use the model builder without regulisation
  trainPredictors = trainPredictors, # pass all training predictors
  trainResponse = trainResponse, # pass all training responses
  k = 4, # Define # folds for k folds cross validation
  layers = c(2,4,8,16), # define # layers to test
  units = c(4,8,16,20), # define # units to test
  batch = c(4,8,16,20), # batch size to trial
  e = 300, # define number of epochs (note --> callback is used so rarley will this number be achieved)
  delta = .001, # set delta for call back end training
  patience = 5, # set patience for call back end training
  dropout = 0, # regulations parameter, not used in this tune
  aim = 1 # set aim to return the results gris (ie train and Val MAE)
)

## record finish time
finishTime <- Sys.time()
runTime_ct <- finishTime - startTime
#####
## save resultsGrid to RDS for use later
saveRDS(resultsGrid,"resultsGrid_NN_course_gloud.RDS")

## Fine Tune
#####
## record start time
## record start time
startTime <- Sys.time()

resultsGrid <- test_tune_grid( # call test tune grid to build and test model with the following parameters
  model_builder = build_model, # use the model builder without regulisation
  trainPredictors = trainPredictors, # pass all training predictors
  trainResponse = trainResponse, # pass all training responses
  k = 4, # Define # folds for k folds cross validation
  layers = c(6,7,8,9,10,11,12), # define # layers to test
  units = c(16,17,18,19,20), # define # units to test
  batch = c(1, 2, 4), # batch size to trial
  e = 500, # define number of epochs (note --> callback is used so rarley will this number be achieved)
  delta = .000005, # set delta for call back end training
  patience = 10, # set patience for call back end training
  dropout = 0, # regulations parameter, not used in this tune
  aim = 1 # set aim to return the results gris (ie train and Val MAE)
)

## record finish time
finishTime <- Sys.time()
runTime_ft <- finishTime - startTime
#####
## save resultsGrid to RDS for use later
saveRDS(resultsGrid,"resultsGrid_NN_fine_gcloud.RDS")

## Investigate regulization
#####
## record start time
## record start time
startTime <- Sys.time()

resultsGrid <- test_tune_grid( # call test tune grid to build and test model with the following parameters
  model_builder = build_model_reg, # use the model builder without regulisation
  trainPredictors = trainPredictors, # pass all training predictors
  trainResponse = trainResponse, # pass all training responses
  k = 4, # Define # folds for k folds cross validation
  layers = c(11), # freeze layers from previous investigation
  units = c(19), # freeze units from previous investigation
  batch = c(2), # freeze batch from previous investigation
  e = 500, # define number of epochs (note --> callback is used so rarley will this number be achieved)
  delta = .00000005, # set delta for call back end training
  patience = 10, # set patience for call back end training
  dropout = c(0.2,0.4,0.6), # regulations parameter
  aim = 1 # set aim to return the results gris (ie train and Val MAE)
)

## record finish time
finishTime <- Sys.time()
runTime_reg <- finishTime - startTime
#####
## save resultsGrid to RDS for use later
saveRDS(resultsGrid,"resultsGrid_NN_reg_gcloud.RDS")

## Build 2 final models for comparison and assessment one with drop out one with out
#####
## record start time
startTime <- Sys.time()

## No reg
history_19_11 <- test_tune_grid( # call test tune grid to build and test model with the following parameters
  model_builder = build_model, # use the model builder without regulisation
  trainPredictors = trainPredictors, # pass all training predictors
  trainResponse = trainResponse, # pass all training responses
  k = 4, # Define # folds for k folds cross validation
  layers = c(11), # freeze layers from previous investigation
  units = c(19), # freeze units from previous investigation
  batch = c(2), # freeze batch from previous investigation
  e = 500, # define number of epochs (note --> callback is used so rarley will this number be achieved)
  delta = 0, # set delta for call back end training
  patience = 500, # effectively turn off stop early
  dropout = c(0), # regulations parameter
  aim = 2 # set aim to return the results gris (ie train and Val MAE)
)

## save history_19_11 to RDS for use later
saveRDS(history_19_11,"history_19_11.RDS")

## Reg
history_19_11_reg <- test_tune_grid( # call test tune grid to build and test model with the following parameters
  model_builder = build_model_reg, # use the model builder without regulisation
  trainPredictors = trainPredictors, # pass all training predictors
  trainResponse = trainResponse, # pass all training responses
  k = 4, # Define # folds for k folds cross validation
  layers = c(11), # freeze layers from previous investigation
  units = c(19), # freeze units from previous investigation
  batch = c(2), # freeze batch from previous investigation
  e = 500, # define number of epochs (note --> callback is used so rarley will this number be achieved)
  delta = 0, # set delta for call back end training
  patience = 500, # effectively turn off stop early
  dropout = c(0.2), # regulations parameter
  aim = 2 # set aim to return the results gris (ie train and Val MAE)
)

## record finish time
finishTime <- Sys.time()
runTime_ft <- finishTime - startTime
#####
## save history_19_11 to RDS for use later
saveRDS(history_19_11_reg,"history_19_11_reg.RDS")

## Build final model epochs = 50, no regulation and fit to training data, then test on test data
#####
## Build Model
tensorboard("my_log_dir") # call tensor board

callbacks = list( # only call back TB, no need to stop training early as we have selected the desired number of epochs
  callback_tensorboard( # call TB
    log_dir = "my_log_dir",
    histogram_freq = 1))

model_final <- build_model( # use the model builder without regulisation to build model
  l = 11, # build model with 11 layers
  u = 19,  # build model with 19 units
  is = ncol(trainPredictors), # input shape 
  opt = "adam", # optimiser
  loss = "mse", # loss function
  met = "mae", # metrics
  dr = NA # dropout is NA as regulisation is not being used
)

## Fit Model and story history
history <- model_final %>% fit( # fit model and record results in history
  trainPredictors, # training predictors
  trainResponse, # training response
  epochs = 50, # epochs (predefined for easy adjustment)
  batch_size = 2, # batch size from tuning grid 
  callbacks = callbacks # predefined call backs
)

## return test and training data metrics
resultsTest <- model_final %>% evaluate(testPredictors, testResponse) # calculate fit metrics on test data
resultsTraining <- model_final %>% evaluate(trainPredictors, trainResponse) # calculate fit on training Data

## Predict unemployment using the test and training predictors
plotData_test <- model %>% predict(testPredictors) %>% as.numeric() # return test predictions to new column in plotData_test
trainAss$Predictions <- model %>% predict(trainPred) %>% as.numeric() # return training predictions to new column in plotData Train
plotData_test$Split <- rep("Test", nrow(plotData_test)) # add note that these values are from test split
plotData_train$Split <- rep("Train", nrow(plotData_train)) # add note that these values are from training split

timeSeriesData <- rbind(plotData_train, # combine all predictions into a single dataframe
                        plotData_test)

finalResults <- list(resultsTest, # list all relevant results for export
                     resultsTraining, 
                     timeSeriesData)

saveRDS(finalResults, file = "finalResults_gloud.RDS")
