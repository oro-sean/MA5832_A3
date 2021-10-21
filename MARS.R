# Set up environment and import Data
rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console

library(ggplot2, quietly = TRUE)
library(earth, quietly = TRUE)
library(caret, quietly = TRUE)
library(vip, quietly = TRUE)
library(pdp, quietly = TRUE)
library(data.table, quietly = TRUE)

data <- readRDS(file = "data.RDS") # import model data prepared earlier and saved in RDS file
plotData_test <- as.data.frame(data[5]) # test response and dates
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
                                          nprune = seq(1,20,4) # tune over the range 1 --> 20
                   ))

## Plot results
ggplot(MARS_TUNE) + # quick plot of MAE vs prune and degree
  labs(title = "Accuracy vs # Terms (prune) by Degree") + # add tittle
  theme_light()

## Plot various accuracy measures vs folds to asses metrics
accPlot_data <- as.data.frame(MARS_TUNE$resample) # return accuracy for all folds of the best tune
accPlot_data <- melt(accPlot_data, id.vars = "Resample") # melt data for plotting
accPlot_data$fold <- 1:10 # add fold variable so numeric value for fold
accPlot_stats <- accPlot_data %>% group_by(variable) %>% summarise(Mean = mean(value), SD = sd(value))

accPlot <- ggplot(data = accPlot_data) + # plot accuracy Data
  geom_point(aes(x = fold, y = value, color = variable)) + # plot fold on x axis, value on y axis, colour by metric
  geom_line(aes(x = fold, y = value, color = variable)) + # add line
  labs(title = " Accuracy Vs fold",
       y = "Score",
       x = "Fold") +
  geom_text(aes(x = 2, y = 0.92, label = paste("Mean - ", accPlot_stats$Mean[2], "\n", # add mean and Sd annotations
                                              "SD - ", accPlot_stats$SD[2]))) +
  geom_text(aes(x = 2.5, y = 0.4, label = paste("Mean - ", accPlot_stats$Mean[1], "\n",
                                              "SD - ", accPlot_stats$SD[1]))) +
  geom_text(aes(x = 9, y = 0.15, label = paste("Mean - ", accPlot_stats$Mean[3], "\n",
                                                "SD - ", accPlot_stats$SD[3]))) +
  theme_light()
accPlot

## Fit final model
set.seed(123)
MARS_REFINED <- train(x = trainPredictors, # use caret train to train the model on training predictors
                      y = trainResponse, # training response
                      method = "earth", # use the MARS algorithm
                      metric = "Rsquared", # train using Rsquared
                      trControl = trainControl( method = "LOOCV", # use LOOCV to help reduce influence of outliers and overfitting
                                                number = 1), # # to leave out
                      tuneGrid = expand.grid(degree = 1:2, # tune for degree 1 --> 2
                                             nprune = 15:21) # tune over the range 10 --> 25
)

## Plot results
ggplot(MARS_REFINED) + # quick plot of MAE vs prune and degree
  labs(title = "Accuracy vs # Terms (prune) by Degree") + # add tittle
geom_vline(xintercept = MARS_REFINED$bestTune[[1]]) + # draw vertical line at best nprune
  geom_text(aes(x = 20, y = .978, label = paste("nprune = ", MARS_REFINED$bestTune[[1]]))) +
  theme_light()

## Return final hyper parameters
MARS_REFINED$bestTune

## Plot various accuracy measures vs folds for final model
accPlot_data_final <- as.data.frame(MARS_REFINED$resample) # return accuracy for all folds of the best tune

accPlot_data_final <- melt(accPlot_data_final, id.vars = "Resample") # melt data for plotting
accPlot_data_final$fold <- 1:9 # add fold variable so numeric value for fold
accPlot_stats_final <- accPlot_data_final %>% group_by(variable) %>% summarise(Mean = mean(value), SD = sd(value))

accPlot_final <- ggplot(data = accPlot_data_final) + # plot accuracy Data
  geom_point(aes(x = fold, y = value, color = variable)) + # plot fold on x axis, value on y axis, colour by metric
  geom_line(aes(x = fold, y = value, color = variable)) + # add line
  labs(title = " Accuracy Vs fold",
       y = "Score",
       x = "Fold") +
  geom_text(aes(x = 2, y = 0.92, label = paste("Mean - ", accPlot_stats_final$Mean[2], "\n", # add mean and Sd annotations
                                               "SD - ", accPlot_stats_final$SD[2]))) +
  geom_text(aes(x = 2.5, y = 0.3, label = paste("Mean - ", accPlot_stats_final$Mean[1], "\n",
                                                "SD - ", accPlot_stats_final$SD[1]))) +
  geom_text(aes(x = 9, y = 0.14, label = paste("Mean - ", accPlot_stats_final$Mean[3], "\n",
                                               "SD - ", accPlot_stats_final$SD[3]))) +
  theme_light()
accPlot_final

## Plot predicted v actual
plotData_test$Predictions <- predict(MARS_REFINED$finalModel, testPredictors) # add model predictions for test data to plotData_tets df
plotData_train$Predictions <- MARS_REFINED$finalModel$fitted.values # add model predictions for training data to plotDatat_train df
plotData_test$Split <- rep("Test", nrow(plotData_test)) # add note that these values are from test split
plotData_train$Split <- rep("Train", nrow(plotData_train)) # add note that these values are from training split
timeSeriesData <- rbind(plotData_train, # combine all predictions into a single data frame
                        plotData_test)
timeSeriesData$Split <- as.factor(timeSeriesData$Split) # make Split a factor

ts_plot <- ggplot(data = timeSeriesData) + # use time series data to make plot
  geom_line(aes(x = Date, y = Y), colour = "blue") + # plot actual unemployment in blue
  geom_line(aes(x = Date, y = Predictions, color = Split)) +
  labs(title = "Actual vs Predicted Employment", y = "Unemployment (%)", x = "Date") + # add tittle and axis labels
  geom_text(aes(x = Date[60], y = 8.5, label = paste("Training MAE = ", MAE(plotData_train$Y, plotData_train$Predictions)))) +  # text stating MAE of training set
  geom_text(aes(x = Date[125], y = 4, label = paste("Test MAE = ", MAE(plotData_test$Y, plotData_test$Predictions)))) + # text MAE of test set
  theme_light()
  ts_plot

## Plot variable importance
vip(MARS_REFINED$finalModel)
vi_scores <- vi(MARS_REFINED$finalModel)
varImp_plot <- ggplot(data = vi_scores, aes(x = Importance, y = Variable)) +
  geom_col()
varImp_plot
vi_scores$Importance

partial(MARS_TUNE, pred.var = "T5_mean_Window_19_lagged_0") %>% autoplot() 
summary(MARS_REFINED$finalModel)


