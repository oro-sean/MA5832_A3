## set up environment and import data file

rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(readxl, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(DataExplorer, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(R.utils, quietly = TRUE)
library(data.table, quietly = TRUE)
library(tseries, quietly = TRUE)
library(roll, quietly = TRUE)
library(imputeFin, quietly = TRUE)
library(earth, quietly = TRUE)

## Import Data from .xlsx
file <- "AUS_Data.xlsx" # set source data file name
dataIn <-  read_excel(file,
                       col_name = TRUE, # first row is column names
                       col_types = rep(c("numeric"), times = 9) # import all variables as numeric
                       ) # the data is imported, the .csv file has headers which will be used as the column names of the data frame, any stirngs in the data set will be treated as factors

rawData <- data.frame(dataIn[-1 , ]) # change data structure to dataframe drop the first row as it was the column names.

rawData[ ,1] <- as.Date(as.POSIXct(rawData[ ,1]*24*60*60, origin = "1900-01-01")) # convert excel time to POSIXct excel time is days since 1990, POSIXct excects seconds from origin
colnames(rawData)[1] <- c("Date") # set column name


## exploratory visulisation

introduce(rawData) # quick tables detailing data
plot_missing(rawData) # find missing values

## Impute missing values using
(is.na(rawData$X6))
plot(x = rawData$Date, y = rawData$X6) # values are in the middle and no clear trend so we will use Gaussian mixture
X6_plot <- plot_imputed(impute_AR1_Gaussian(rawData$X6))
X6_plot # results seem reasonable
rawData$X6 <- impute_AR1_Gaussian(rawData$X6) # impute values
attr(rawData$X6, "index_miss") <- NULL # remove attribute from data frome for simplicity

(is.na(rawData$X7))
plot(x = rawData$Date, y = rawData$X7) # values are at the end and part of a stable linear trend so we will impute with linear extrapolation
X7_NA <- lm(X7 ~ Date, 
            data = rawData[year(rawData$Date) > 2010 & !is.na(rawData$X7), ]) # fit linear model on data after 2010 and not missing
for(i in 1:nrow(rawData)){
  if(is.na(rawData$X7[i])){
    rawData$X7[i] = predict.lm(X7_NA, newdata = data.frame(Date = rawData$Date[i]))
  }
}

plot(x = rawData$Date, y = rawData$X7)

plotData <- rawData # move data to new Df for manipulation for plotting
colnames(plotData) <- c("Date",
                        "Y - Unemployment Rate (Perc)", 
                        "X1 - Change in GDP (Perc)",
                        "X2 - Change in Consumption Expenditure (gov) (Perc)", 
                        "X3 - Change in Consumption Expenditure (All) (Perc)", 
                        "X4 - Term of Trade Index (Perc)",
                        "X5 - CPI",
                        "X6 - # Job Vacancies (Thousands)",
                        "X7 - Estimated Resident Population (Thousands)")

plotData[ ,-1] <- scale(plotData[ , -1], center = TRUE, scale = TRUE) # scale all values except the period 
plotData <- melt( plotData, # Melt Data for plotting
  id.vars = c("Date"), # use Periods as ID's
  varnames = c("Date", "variable", "value"), # Set column names
  na.rm = TRUE) # remove missinng values for the time being



timeSeries_plot_init <- ggplot(data = plotData, aes(x = Date, y = value)) + # plot Data
  geom_line(aes(color = variable), size = 1) + # add line showing data
  geom_smooth(colour = "darkgrey", method = "loess", span = 0.2, alpha = .75, se = FALSE, linetype = "longdash") + # add smoothed line to asses long term trends
  facet_wrap(plotData$variable) + 
  theme_classic()
  
timeSeries_plot_init

## we can see that job vacancies and population continue to grow - not great comparision as they are absolute
rawData$T1 <- insert(diff(rawData$X6), 1, values = NA) / rawData$X6 * 100 # create T1 variable % change to X6 (job vacancies)

rawData <- rawData %>% mutate(T2 = X6 / X7 * 100) %>% # create T2 Variable X6 (job vacancies) normalised for population
  mutate(T3 = ifelse(Date < as.Date("2000-07-01"), X5 * 1.1, X5)) # create T3 Correct CPI for GST

rawData$T4 <- insert(diff(rawData$X5), 1, values = NA) / rawData$X5 * 400 # create

plotData <- rawData

colnames(plotData) <- c("Date",
                        "Y - Unemployment Rate (Perc)", 
                        "X1 - Change in GDP (Perc)",
                        "X2 - Change in Consumption Expenditure (gov) (Perc)", 
                        "X3 - Change in Consumption Expenditure (All) (Perc)", 
                        "X4 - Term of Trade Index (Perc)",
                        "X5 - CPI",
                        "X6 - # Job Vacancies (Thousands)",
                        "X7 - Estimated Resident Population (Thousands)",
                        "T1 - Change in Job Vacancies (Perc)",
                        "T2 - Job Vacancy (Perc of Population)",
                        "T3 - CPI corrected for GST",
                        "T4 - Quarterly inflation Anulized")

plotData[ ,-1] <- scale(plotData[ , -1], center = TRUE, scale = TRUE) # scale all values except the period 
plotData <- melt( plotData, # Melt Data for plotting
                  id.vars = c("Date"), # use Periods as ID's
                  varnames = c("variable", "value"), # Set column names
                  na.rm = TRUE) # remove missinng values for the time being

timeSeries_plot <- ggplot(data = plotData, aes(x = Date, y = value)) + # plot Data
  geom_line(aes(color = plotData$variable), size = 1) + # add line showing data
  geom_smooth(colour = "darkgrey", method = "loess", span = 0.2, alpha = .75, se = FALSE, linetype = "longdash") + # add smoothed line to asses long term trends
  facet_wrap(plotData$variable) + 
  theme_classic()

timeSeries_plot

## Inspect Data for seasonality
rawData <- rawData %>% mutate(season = month(Date)/3, .after = Date) %>% # create season variable as 1 = summer 2 = autumn 3 = winter 4 = spring
  mutate(year = year(Date), .after = Date)

plotData <- rawData

# colnames(plotData) <- c("Date",
#                         "Year",
#                         "Season",
#                         "Y - Unemployment Rate (Perc)", 
#                         "X1 - Change in GDP (Perc)",
#                         "X2 - Change in Consumption Expenditure (gov) (Perc)", 
#                         "X3 - Change in Consumption Expenditure (All) (Perc)", 
#                         "X4 - Term of Trade Index (Perc)",
#                         "X5 - CPI",
#                         "X6 - # Job Vacancies (Thousands)",
#                         "X7 - Estimated Resident Population (Thousands)",
#                         "T1 - Change in Job Vacancies (Perc)",
#                         "T2 - Job Vacancy (Perc of Population)",
#                         "T3 - CPI corrected for GST",
#                         "T4 - Quarterly inflation Annualized"
#                         )
plotData <- plotData[ , c(2,3,4)] # omit variables that ahve been superceeded by nomralized variables

plotData <- plotData %>% pivot_wider(names_from = season, values_from = Y) %>% 
  rowwise() %>% mutate(min = min(`1`,`2`,`3`,`4`), 
                       max = max(`1`,`2`,`3`,`4`))

plotData <- melt( plotData, # Melt Data for plotting
                  id.vars = c("year", "min", "max"), # use Date, year and season as ID's
                  varnames = c("Season", "value"), # Set column names
                  na.rm = TRUE) # remove missing values for the time being
plotData <- plotData[order(plotData$year, plotData$variable), ]
plotData <- plotData %>% mutate(`Unemployment (Perc) (Standardized Annually)` = (value-min)/(max-min),
                                label = row_number(year))


seasonal_plot <- ggplot(data = plotData, aes(x = label, y = `Unemployment (Perc) (Standardized Annually)`)) +
  geom_line() +
  geom_point(aes(color = as.factor(variable))) +
  theme_classic()

seasonal_plot

plotData <- rawData

# colnames(plotData) <- c("Date",
#                         "Year",
#                         "Season",
#                         "Y - Unemployment Rate (Perc)", 
#                         "X1 - Change in GDP (Perc)",
#                         "X2 - Change in Consumption Expenditure (gov) (Perc)", 
#                         "X3 - Change in Consumption Expenditure (All) (Perc)", 
#                         "X4 - Term of Trade Index (Perc)",
#                         "X5 - CPI",
#                         "X6 - # Job Vacancies (Thousands)",
#                         "X7 - Estimated Resident Population (Thousands)",
#                         "T1 - Change in Job Vacancies (Perc)",
#                         "T2 - Job Vacancy (Perc of Population)",
#                         "T3 - CPI corrected for GST",
#                         "T4 - Quarterly inflation Annualized"
#                         )
plotData <- plotData[ , c(2,3,4)] # omit variables that ahve been superceeded by nomralized variables

## std over entire time
plotData <- rawData

# colnames(plotData) <- c("Date",
#                         "Year",
#                         "Season",
#                         "Y - Unemployment Rate (Perc)", 
#                         "X1 - Change in GDP (Perc)",
#                         "X2 - Change in Consumption Expenditure (gov) (Perc)", 
#                         "X3 - Change in Consumption Expenditure (All) (Perc)", 
#                         "X4 - Term of Trade Index (Perc)",
#                         "X5 - CPI",
#                         "X6 - # Job Vacancies (Thousands)",
#                         "X7 - Estimated Resident Population (Thousands)",
#                         "T1 - Change in Job Vacancies (Perc)",
#                         "T2 - Job Vacancy (Perc of Population)",
#                         "T3 - CPI corrected for GST",
#                         "T4 - Quarterly inflation Annualized"
#                         )
plotData <- plotData[ , c(2,3,4)] # omit variables that ahve been superceeded by nomralized variables
plotData$Y <- insert(diff(plotData$Y), 1, values = NA) / plotData$Y * 100
Ymin <- min(plotData$Y, na.rm = TRUE)
Ymax <- max(plotData$Y, na.rm = TRUE)
#plotData$Ystd <- (plotData$Y - Ymin) / (Ymax - Ymin)
plotData$Ystd <- plotData$Y

plotData <- melt( plotData[ ,-3], # Melt Data for plotting
                  id.vars = c("year", "season"),
                  na.rm = TRUE) # remove missing values for the time being
plotData <- plotData[order(plotData$year, as.numeric(plotData$season)), ]
plotData$label <- row_number(plotData$year)



seasonal_plot <- ggplot(data = plotData, aes(x = season, y = value)) +
  geom_line(aes(color = as.factor(year))) +
  geom_point(aes(color = as.factor(year))) +
  theme_classic()

seasonal_plot

### Investigate possible correlations

plotData <- rawData # move data to new Df for manipulation for plotting
colnames(plotData) <- c("Date",
                                                "Year",
                                                "Season",
                                                "Y - Unemployment Rate (Perc)",
                                                "X1 - Change in GDP (Perc)",
                                                "X2 - Change in Consumption Expenditure (gov) (Perc)",
                                                "X3 - Change in Consumption Expenditure (All) (Perc)",
                                                "X4 - Term of Trade Index (Perc)",
                                                "X5 - CPI",
                                                "X6 - # Job Vacancies (Thousands)",
                                                "X7 - Estimated Resident Population (Thousands)",
                                                "T1 - Change in Job Vacancies (Perc)",
                                                "T2 - Job Vacancy (Perc of Population)",
                                                "T3 - CPI corrected for GST",
                                                "T4 - Quarterly inflation Annualized"
                                                )
plotData <- plotData[ ,c(-9,-10,-11,-14)]
plotData[ ,c(-1,-2,-3)] <- scale(plotData[ , c(-1,-2,-3)], center = TRUE, scale = TRUE) # scale all values except the period 
plotData <- melt( plotData, # Melt Data for plotting
                  id.vars = c("Date", "Year", "Season"), # use Periods as ID's
                  varnames = c("Date", "variable", "value"), # Set column names
                  na.rm = TRUE) # remove missinng values for the time being

corplot <- ggplot(data = plotData, aes(x = Date, y = value)) + # plot Data
  geom_smooth(aes(color = variable), method = "loess", span = 0.2, alpha = .75, se = FALSE) + # add smoothed line to asses long term trends
  theme_classic()
corplot

## Look for lag correlation
plotData <- rawData # move data to new Df for manipulation for plotting
# colnames(plotData) <- c("Date",
#                         "Year",
#                         "Season",
#                         "Y - Unemployment Rate (Perc)",
#                         "X1 - Change in GDP (Perc)",
#                         "X2 - Change in Consumption Expenditure (gov) (Perc)",
#                         "X3 - Change in Consumption Expenditure (All) (Perc)",
#                         "X4 - Term of Trade Index (Perc)",
#                         "X5 - CPI",
#                         "X6 - # Job Vacancies (Thousands)",
#                         "X7 - Estimated Resident Population (Thousands)",
#                         "T1 - Change in Job Vacancies (Perc)",
#                         "T2 - Job Vacancy (Perc of Population)",
#                         "T3 - CPI corrected for GST",
#                         "T4 - Quarterly inflation Annualized"
# )
plotData <- plotData[ ,c(-9,-10,-11,-14)]
plotData[ ,c(-1,-2,-3)] <- scale(plotData[ , c(-1,-2,-3)], center = TRUE, scale = TRUE) # scale all values except the period
plotData <- plotData[complete.cases(plotData), ]


test <- ccf(plotData$X4, plotData$Y) #-5 -.25
ccf(plotData$T1, plotData$Y) #-7 -.2
ccf(plotData$T2, plotData$Y) #-3 -.8
ccf(plotData$T4, plotData$Y) #-7 -.3

## investigate corelation with variance

plotData <- rawData
plotData$x1var4 <- roll_var(plotData$X1, width = 4)
plotData$x1var6 <- roll_var(plotData$X1, width = 6) #
plotData$x1var8 <- roll_var(plotData$X1, width = 8) #
plotData$x2var4 <- roll_var(plotData$X2, width = 4) #
plotData$x2var6 <- roll_var(plotData$X2, width = 6)
plotData$x2var8 <- roll_var(plotData$X2, width = 8) #

plotData <- plotData[complete.cases(plotData), ]

ccf(plotData$x1var4, plotData$Y) 
ccf(plotData$x1var6, plotData$Y) #11 .35
ccf(plotData$x1var8, plotData$Y) #0 .6

ccf(plotData$x2var4, plotData$Y) #7 .6
ccf(plotData$x2var6, plotData$Y) 
ccf(plotData$x2var8, plotData$Y) #-5 .65


## detrend population growth
POP_TREND <- lm(X7 ~ Date, data = rawData)

rawData$T5 <- rawData$X7 - unname(predict.lm(POP_TREND, newdata = data.frame(Date = rawData$Date)))

plot(x = rawData$Date, y = rawData$T5)

ccf(rawData$T5, rawData$Y, lag.max = 60) #-35 .6

longest <- nrow(rawData)+35

modelData = data.frame(
T1 = lag(rawData$T1, 7),
T2 = lag(rawData$T2, 3),
T4 = lag(rawData$T4, 7),
T5 = lag(rawData$T5, 35),
x1var6 = lag(roll_var(rawData$X1, width = 6), 11), 
x1var8 = roll_var(rawData$X1, width = 8), 
x2var4 = lag(roll_var(rawData$X2, width = 4), 7), 
x2var8 = lag(roll_var(rawData$X2, width = 8), 5), 
response = rawData$Y,
Date = rawData$Date)

modelData <- modelData[complete.cases(modelData), ]

results <- data.frame(Date =modelData[year(modelData$Date) > 2018, 10])
trainAss <- data.frame(Date = modelData[year(modelData$Date) <= 2018, 10])

testData <- modelData[year(modelData$Date) > 2018, -10]
trainData <- modelData[year(modelData$Date) <= 2018, -10]

mean <- apply(trainData[ , -9], 2, mean)
std <- apply(trainData[ , -9], 2, sd)

testPred = scale(testData[ ,-9], center = mean, scale = std)
testResponse = testData[,9]

results$Actual <- testResponse

trainPred = scale(trainData[ ,-9], center = mean, scale = std)
trainResponse = trainData[,9] 

trainAss$Actual <- trainResponse

MOD <- earth(x = trainPred, y = trainResponse, pmethod = "cv", ncross = 3, nfold = 5)
summary(MOD)
results$Predictions <- predict(MOD, newdata = testPred)
trainAss$Predictions <- predict(MOD)
plot(MOD)

ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Actual), colour = "blue") 
ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Actual), colour = "blue") 

out <- predict(MOD)
trainResponse

saveRDS(testResponse, file = "testResponse.RDS")
saveRDS(testPred, file = "testPred.RDS")
saveRDS(trainResponse, file = "trainResponse.RDS")
saveRDS(trainPred, file = "trainPred.RDS")
