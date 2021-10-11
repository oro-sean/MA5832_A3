rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR() # restart r session to disocnect any old python attachments

library(keras, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tensorflow, quietly = TRUE)
library(reticulate, quietly = TRUE)
library(caret, quietly = TRUE)
library(dplyr)

## Set env variables and check Py
Sys.setenv(RETICULATE_PYTHON = "/home/veering/anaconda3/envs/r-reticulate/bin/python") # for reticulate to call correct python environemnt
Sys.getenv("RETICULATE_PYTHON") # check Sys variable is correct
py_config() # check python config

## Set seed for keras
set_random_seed(123)

## read Training Data in

data <- readRDS(file = "data.RDS") # import model data prerpared earlier and daved in RDS file
plotData_test <- as.data.frame(data[5]) # test reponses and dates
plotData_train <- as.data.frame(data[6]) # train response and dates
testPredictors <- as.matrix(as.data.frame(data[7])) # test predictors, matrix for tf
testResponse <- as.numeric(unlist(data[8])) # test response, numeric vec for tf
trainPredictors <- as.matrix(as.data.frame(data[9])) # training predictors, as matrix for tf
trainResponse <- as.numeric(unlist(data[10])) # training response, as matrix for tf
resultsGrid_course <-readRDS(file = "resultsGrid_NN_course.RDS") # import course training results

names(resultsGrid_course) <- c("Fold", "Layers", "Units", "Batch Size","tg", "k", "MAE Train", "MAE Val", "# Epochs") # give training grid results meaning fulname

avgMAE_c <- resultsGrid %>% group_by(Layers, Units, `Batch Size`) %>%  # group by layer, unit and batch size
filter(Units != 4) %>% # from inspection 4 units was unmeaningfully inaccurate so omit at this step to make plots clear
  summarize(`MAE Train` = mean(`MAE Train`), `MAE Val` = mean(`MAE Val`)) # calculate average (over the k folds) MAE for training and validation set for each combination of layers, units and batch size
avgMAE_c$Units <- as.factor(avgMAE_c$Units) # make Units a factor for plotting
avgMAE_c$`Batch Size` <- as.factor(avgMAE_c$`Batch Size`) # make 

gridCourse_plot <- ggplot(data = avgMAE_c) + 
  geom_point(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add points for each value of units
  geom_line(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add lines for each value of units
  geom_point(aes(x = Layers, y = `MAE Val`, colour = Units)) +  # repeat for validation MAE
  geom_line(aes(x = Layers, y = `MAE Val`, colour = Units), linetype = "dashed") +
  facet_wrap(as.factor(avgMAE_c$`Batch Size`)) + # facet wrap Batch size
  labs(title = "MAE vs Layers by Units and Batch Size", Y = "Mean Absolute Error (MAE)") +
  theme_classic()
gridCourse_plot


