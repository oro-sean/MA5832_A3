rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console

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
gridPlot_course <- GRID_PLOT(readRDS(file = "resultsGrid_NN_course_gloud.RDS"))
gridPlot_course
## plot the results from the fine tuining grid
gridPlot_fine <- GRID_PLOT(readRDS(file = "resultsGrid_NN_fine_gcloud.RDS"))
gridPlot_fine


## plot results for regulization
resultsGrid<- as.data.frame(readRDS(file = "resultsGrid_NN_reg_gcloud.RDS")) # import results from RDS
names(resultsGrid) <- c("Fold", "Layers", "Units", "Batch Size", "Drop Out Rate", "tg", "k", "MAE Train", "MAE Val", "# Epochs") # give training grid results meaning fulname
resultsGrid$`Drop Out Rate` <- as.factor(resultsGrid$`Drop Out Rate`) # make dr a factor

avgMAE <- resultsGrid %>% group_by(Layers, `Drop Out Rate`) %>%  # group by layer, unit and drop out rate
  summarize(`MAE Train` = mean(`MAE Train`), `MAE Val` = mean(`MAE Val`), epoch = median(`# Epochs`)) # calculate average (over the k folds) MAE for training and validation set for each combination of layers, units and batch size

reg_plot <- ggplot(data = avgMAE) + # Create Plot
  geom_point(aes(x = Layers, y = `MAE Train`, color = `Drop Out Rate`)) + 
  geom_line(aes(x = Layers, y = `MAE Train`, color = `Drop Out Rate`)) + 
  geom_point(aes(x = Layers, y = `MAE Val`, color = `Drop Out Rate`)) + 
  geom_line(aes(x = Layers, y = `MAE Val`, color = `Drop Out Rate`), linetype = "dashed") +
  geom_text(aes(x = avgMAE$Layers, y = 1.6, label = paste("Median # epochs - ", "\n", median(avgMAE$epoch))))+
  labs(title = "MAE vs Layers for various drop out rates", Y = "Mean Absolute Error (MAE)") +
  
  theme_light()
reg_plot

## plot mae vs epoch for reg and no reg
HIST_TO_PLOT <- function(history, reg){
historyVal <- history[[1]] # unlist validation and training MAE
historyTrain <- history[[2]]
plotData <- data.frame(epochs = seq(1:ncol(historyVal)), # create epochs variable as seq to end of mae
                       regulisation = rep(reg, ncol(historyVal)), # add note that these are unregulised
                       Validation = apply(historyVal, 2, mean), # average validation MAE over all folds
                       Training = apply(historyTrain, 2, mean)) # average training MAE over all folds
}

plotData <- rbind( # Build plot Data Data frame by combining both regulated and uin regulated model history
  HIST_TO_PLOT( # call function to average valdiation and training MAE's
    readRDS(file = "history_21_12.RDS"), # import list of training and validation MAE's
    "Without Regulation"), # specify no regulation
  HIST_TO_PLOT( # call function to average over folds
    readRDS(file = "history_20_36_reg_04.RDS"), # read regulated model history
            "With Dropout Regulation") # Specify Regulated 
)
plotData <- melt(plotData, id.vars = c("epochs", "regulisation")) # melt Data for plotting
plotData <- plotData %>% filter(epochs <= 250)

epochs_plot_comb <- ggplot(plotData, aes(x = epochs, y = value, colour = variable)) + # plot with GG
  geom_smooth() + # add smoothing
  facet_wrap(plotData$regulisation) + # facet wrap regulisation
  theme_classic() +
  labs(title = "MAE Vs # Epochs for net with and without regulisation", 
       y = "Mean Absolute error (MAE)",
       x = "# Epochs")

epochs_plot_comb 

## Plot final time series

finalResults <- readRDS(file = "finalResults_gcloud.RDS") # import final results RDS
MAEtest <- finalResults[[1]] # extract test MAE
MAEtrain <- finalResults[[2]] # extract training MAE
timeSeries <- finalResults[[3]] # extract time series predictions
timeSeries$Split <- as.factor(timeSeries$Split) # make Split a factor

ts_plot <- ggplot(data = timeSeries) + # use time series data to make plot
  geom_line(aes(x = Date, y = Y), colour = "blue") + # plot actual unemployment in blue
  geom_line(aes(x = Date, y = Predictions, color = Split)) +
  theme_light()
ts_plot
