rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR() # restart r session to disocnect any old python attachments

library(ggplot2, quietly = TRUE)
library(dplyr)

## define function to plot tuining grids
GRID_PLOT <- function(resultsGrid){ # function to plot results from nn tuning grid
names(resultsGrid) <- c("Fold", "Layers", "Units", "Batch Size","tg", "k", "MAE Train", "MAE Val", "# Epochs") # give training grid results meaning fulname

avgMAE <- resultsGrid %>% group_by(Layers, Units, `Batch Size`) %>%  # group by layer, unit and batch size
filter(Units != 4) %>% # from inspection 4 units was woefully inaccurate so omit at this step to make plots clear
  summarize(`MAE Train` = mean(`MAE Train`), `MAE Val` = mean(`MAE Val`)) # calculate average (over the k folds) MAE for training and validation set for each combination of layers, units and batch size
avgMAE$Units <- as.factor(avgMAE$Units) # make Units a factor for plotting
avgMAE$`Batch Size` <- as.factor(avgMAE$`Batch Size`) # make 

grid_plot <- ggplot(data = avgMAE) + 
  geom_point(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add points for each value of units
  geom_line(aes(x = Layers, y = `MAE Train`, colour = Units)) + # add lines for each value of units
  geom_point(aes(x = Layers, y = `MAE Val`, colour = Units)) +  # repeat for validation MAE
  geom_line(aes(x = Layers, y = `MAE Val`, colour = Units), linetype = "dashed") +
  facet_wrap(as.factor(avgMAE$`Batch Size`)) + # facet wrap Batch size
  labs(title = "MAE vs Layers by Units and Batch Size", Y = "Mean Absolute Error (MAE)") +
  theme_classic()
grid_plot
}

## plot the results from the corse tuning grid
gridPlot_course <- GRID_PLOT(readRDS(file = "resultsGrid_NN_course_ubuntu.RDS"))

## plot the results from the fine tuining grid
gridPlot_fine <- GRID_PLOT(readRDS(file = "resultsGrid_NN_fine_gcloud.RDS"))
