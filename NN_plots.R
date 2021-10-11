rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR() # restart r session to disocnect any old python attachments

library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(reshape2, quietly = TRUE)

## define function to plot tuining grids
GRID_PLOT <- function(resultsGrid){ # function to plot results from nn tuning grid
  names(resultsGrid) <- c("Fold", "Layers", "Units", "Batch Size", "Drop Out Rate", "tg", "k", "MAE Train", "MAE Val", "# Epochs") # give training grid results meaning fulname
  
  ## Sumarise Data
  avgMAE <- resultsGrid %>% group_by(Layers, Units, `Batch Size`) %>%  # group by layer, unit and batch size
    filter(Units != 4) %>% # from inspection 4 units was woefully inaccurate so omit at this step to make plots clear
    summarize(`MAE Train` = mean(`MAE Train`), `MAE Val` = mean(`MAE Val`)) # calculate average (over the k folds) MAE for training and validation set for each combination of layers, units and batch size
  avgMAE$Units <- as.factor(avgMAE$Units) # make Units a factor for plotting
  avgMAE$`Batch Size` <- as.factor(avgMAE$`Batch Size`) # make 
  
  ## Create Plot
  grid_plot <- ggplot(data = avgMAE) + 
    geom_point(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add points for each value of units
    geom_line(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add lines for each value of units
    geom_point(aes(x = Layers, y = `MAE Val`, colour = Units)) +  # repeat for validation MAE
    geom_line(aes(x = Layers, y = `MAE Val`, colour = Units), linetype = "dashed") +
    facet_wrap(as.factor(avgMAE$`Batch Size`)) + # facet wrap Batch size
    labs(title = "MAE vs Layers by Units and Batch Size", Y = "Mean Absolute Error (MAE)") +
    theme_classic()
  
  return(grid_plot)
}

## plot the results from the corse tuning grid
gridPlot_course <- GRID_PLOT(readRDS(file = "resultsGrid_NN_course_ubuntu.RDS"))

## plot the results from the fine tuining grid
gridPlot_fine <- GRID_PLOT(readRDS(file = "resultsGrid_NN_fine_gcloud.RDS"))

## plot results for regulization

resultsGrid<- as.data.frame(readRDS(file = "resultsGrid_NN_reg_gcloud.RDS")) # import results from RDS

names(resultsGrid) <- c("Fold", "Layers", "Units", "Batch Size", "Drop Out Rate", "tg", "k", "MAE Train", "MAE Val", "# Epochs") # give training grid results meaning fulname

avgMAE <- resultsGrid %>% group_by(Layers, Units, `Batch Size`, `Drop Out Rate`) %>%  # group by layer, unit and batch size
  summarize(`MAE Train` = mean(`MAE Train`), `MAE Val` = mean(`MAE Val`)) # calculate average (over the k folds) MAE for training and validation set for each combination of layers, units and batch size

reg_plot <- ggplot(data = avgMAE) + # Create Plot
  geom_point(aes(x = `Drop Out Rate`, y = `MAE Train`), colour = "Red") + # add points for each value of units
  geom_line(aes(x = `Drop Out Rate`, y = `MAE Train`), colour = "Red") + # add lines for each value of units
  geom_point(aes(x = `Drop Out Rate`, y = `MAE Val`), colour = "Green") +  # repeat for validation MAE
  geom_line(aes(x = `Drop Out Rate`, y = `MAE Val`), colour = "Green", linetype = "dashed") +
  labs(title = "MAE vs Drop Out Rate", Y = "Mean Absolute Error (MAE)") +
  theme_classic()
reg_plot

## plot mae vs epoch for reg and no reg
history <- readRDS(file = "history_19_11.RDS") # import data
historyVal <- history[[1]] # unlist validation and training MAE
historyTrain <- history[[2]]
plotData <- data.frame(epochs = seq(1:ncol(historyVal)), # create epochs variable as seq to end of mae 
                       `AVG MAE Validation` = apply(historyVal, 2, mean), # average validation MAE over all folds
                       `AVG MAR Training` = apply(historyTrain, 2, mean)) # average training MAE over all folds
plotData <- melt(plotData, id.vars = "epochs")
ggplot(plotData, aes(x = epochs, y = value, colour = variable)) + geom_smooth()



