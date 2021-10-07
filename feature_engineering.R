## set up environment

rm(list = ls()) # removes all variables
if(!is.null(dev.list())) dev.off() # clear plots
cat("\014") # clear console
.rs.restartR()

library(readxl, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(R.utils, quietly = TRUE)
library(data.table, quietly = TRUE)
library(tseries, quietly = TRUE)
library(roll, quietly = TRUE)
library(imputeFin, quietly = TRUE)
library(foreach, quietly = TRUE)

## Import Data from .xlsx
file <- "AUS_Data.xlsx" # set source data file name
dataIn <-  read_excel(file,
                      col_name = TRUE, # first row is column names
                      col_types = rep(c("numeric"), times = 9) # import all variables as numeric
) # the data is imported, the .csv file has headers which will be used as the column names of the data frame, any stirngs in the data set will be treated as factors

rawData <- data.frame(dataIn[-1 , ]) # change data structure to dataframe drop the first row as it was the column names.

rawData[ ,1] <- as.Date(as.POSIXct(rawData[ ,1]*24*60*60, origin = "1900-01-01")) # convert excel time to POSIXct excel time is days since 1990, POSIXct excects seconds from origin
colnames(rawData)[1] <- c("Date") # set column name

## Input missing values
rawData$X6 <- impute_AR1_Gaussian(rawData$X6) # values are in the middle and no clear trend so we will use Gaussian mixture
attr(rawData$X6, "index_miss") <- NULL # remove attribute from data frome for simplicity

X7_NA <- lm(X7 ~ Date, # values are at the end and part of a stable linear trend so we will impute with linear extrapolation
            data = rawData[year(rawData$Date) > 2010 & !is.na(rawData$X7), ]) # fit linear model on data after 2010 and not missing
for(i in 1:nrow(rawData)){ # cycle through missing values and input using linear model
  if(is.na(rawData$X7[i])){
    rawData$X7[i] = predict.lm(X7_NA, newdata = data.frame(Date = rawData$Date[i]))
  }
}

## Remove trends

rawData$T1 <- insert(diff(rawData$X6), 1, values = NA) / rawData$X6 * 100 # create T1 variable % change period on period to X6

rawData <- rawData %>% mutate(T2 = X6 / X7 * 100) %>% # create T2 Variable X6 normalized for population
  mutate(T3 = ifelse(Date < as.Date("2000-07-01"), X5 * 1.1, X5)) # create T3 Correct CPI for GST

rawData$T4 <- insert(diff(rawData$X5), 1, values = NA) / rawData$X5 * 400 # change X5 (CPI) to inflation, calculated quarterly but annulised

POP_TREND <- lm(X7 ~ Date, data = rawData) # create long term linear model of population growth

rawData$T5 <- rawData$X7 - unname(predict.lm(POP_TREND, newdata = data.frame(Date = rawData$Date))) # remove long term linear trend of population growth

## Calculate rolling trends (mean, standard deviation) for 2 to 20 quarters (6months to 5 years) for each variable

VAR <- function(vec, w, response){
  ## Function to take time series data, calculate the rolling variance of the data over window w
  # then inspect for correlation at lags 0 --> 20 and return lagged variable that had the highest correlation
  var <- roll_var(vec, width = w) # calculate rolling variance
  cor_var <- ccf(var, response, na.action = na.omit, lag.max = 20) # calculate rolling variance for 0 to 20 time periods lg
  lim <- qnorm((1 + 0.95)/2)/sqrt(cor_var$n.used) # calculate 95% confidence interval of correlation
  meaningful <- max(abs(cor_var$acf[1:20])) > lim # check if correlation is significant
  
  if (meaningful){ # if correlation is significant
    correlation <- abs(rev(cor_var$acf[1:20])) # vector of correlation coefficients from lag 0 to 20 (only lag no leading)
    lags <- which(diff(diff(correlation)>=0)<0) # identify local maxima of correlations along the lag
    if (length(lags) != 0){ # if a maxima is found
      max <- which.max(correlation[lags]) # identify which maxima is greatest
      lag <- lags[max] # set lag to the lag corresponding to the highest correlation
      var <- lag(var,lag) # apply lag to the rolling variance data
      cc <- correlation[lag]
      return(list(var,cc,lag))  # return vector and corresponding correlation
    }
    else { # if no maxima is found return the rolling data and correlation for lag = 0
      lag <- 0
      cc<- abs(cor_var$acf[21])
      return(list(var,cc,lag))
    }
  }
  else{
    vec <- rep(NA, length(vec))
    cc <- NA
    lag <- NA
    return(list(vec, cc, lag))
  }
}

ME <- function(vec, w, response){
  ## Function to take time series data, calculate the rolling mean of the data over window w
  # then inspect for correlation at lags 0 --> 20 and return lagged variable that had the highest correlation
  me <- roll_mean(vec, width = w) # calculate rolling variance
  cor_me <- ccf(me, response, na.action = na.omit, lag.max = 20) # calculate rolling mean for 0 to 20 time periods lg
  lim <- qnorm((1 + 0.95)/2)/sqrt(cor_me$n.used) # calculate 95% confidence interval of correlation
  meaningful <- max(abs(cor_me$acf[1:20])) > lim # check if correlation is significant
  
  if (meaningful){ # if correlation is significant
    correlation <- abs(rev(cor_me$acf[1:20])) # vector of correlation coefficients from lag 0 to 20
    lags <- which(diff(diff(correlation)>=0)<0) # identify local maxima of correlations along the lag
    if (length(lags) != 0){ # if a maxima is found
      max <- which.max(correlation[lags]) # identify which maxima is greatest
      lag <- lags[max] # set lag to the lag corresponding to the highest correlation
      me <- lag(me,lag) # apply lag to the rolling mean data
      cc <- correlation[lag]
      return(list(me,cc,lag))  # return vector and corresponding correlation
    }
    else{ # if no maxima is found return the rolling data and correlation for lag = 0
      lag <- 0
      cc<- abs(cor_me$acf[21])
      return(list(me,cc,lag))
    }
  }
  else{
    vec <- rep(NA, length(vec))
    cc <- NA
    lag <- NA
    return(list(vec, cc, lag))
  }
}

VEC <- function(vec, response){

  ## Function to take time series data and inspect for correlation at lags 0 --> 20 and return lagged variable that had the highest correlation
  cor_vec <- ccf(vec, response, na.action = na.omit, lag.max = 20) # calculate rolling mean for 0 to 20 time periods lg
  lim <- qnorm((1 + 0.95)/2)/sqrt(cor_vec$n.used) # calculate 95% confidence interval of correlation
  meaningful <- max(abs(cor_vec$acf[1:20])) > lim # check if correlation is significant
  
  if (meaningful){ # if correlation is significant
    correlation <- abs(rev(cor_vec$acf[1:20])) # vector of correlation coefficients from lag 0 to 20 
    correlation[c(1,20)] <- 0 # set the first correlation to 0 so if it is an acending serris a maxima will always be found
    lags <- which(diff(diff(correlation)>=0)<0) # identify local maxima of correlations along the lag
    if (length(lags) != 0){ # if a maxima is found
      max <- which.max(correlation[lags]) # identify which maxima is greatest
      lag <- lags[max] # set lag to the lag corresponding to the highest correlation
      vec <- lag(vec,lag) # apply lag to the rolling mean data
      cc <- correlation[lag]
      return(list(vec,cc,lag))  # return vector and corresponding correlation
    }
  }
  else{
    vec <- rep(NA, length(vec))
    cc <- NA
    lag <- NA
    return(list(vec, cc, lag))
  }
}

ENG <- function(predictor, response){ # function to take response and predictor and find best rolling and laged combo for mean and variance as
  #well as best lagging and return lagged predictors as a df
  
  varFeatures <- foreach(i = 2:20, .combine = cbind) %do% { # cycle over rolling window 2 --> 20 (6 months to 5 years)
    VAR(predictor, i, response)
  }
  
  meFeatures <- foreach(i = 2:20, .combine = cbind) %do% { # cycle over rolling window 2 --> 20 (6 months to 5 years)
    ME(predictor, i, response)
  }
  
  lagFeatures <- VEC(predictor, response)
  
   n <- ifelse(length(which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)]))) == 0, 1,
          which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)]))*3-2)
  
  varFeat_rol <- varFeatures[n] # assign the rolling feature which has highest correlation
  meFeat_rol  <- meFeatures[which.max(unlist(meFeatures[seq(from = 2, to = length(meFeatures), by = 3)]))*3-2] # assign the rolling feature which has highest correlation
  lagFeat <- lagFeatures[1] # assign best lagged features
  
  varFeat_rol_lag <- varFeatures[which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)]))*3] # assign the lag used for best rolling feature
  meFeat_rol_lag <- meFeatures[which.max(unlist(meFeatures[seq(from = 2, to = length(meFeatures), by = 3)]))*3] # assign the lag used for best rolling feature
  lagFeat_lag <- lagFeatures[3] # record best lag
  
  varFeat_rol_window <- which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)])) # assign the window used for best rolling feature
  meFeat_rol_window <- which.max(unlist(meFeatures[seq(from = 2, to = length(meFeatures), by = 3)])) # assign the window used for best rolling feature
  
  engFeatures <- data.frame(varFeat_rol, meFeat_rol, lagFeat) # group feature vectors together in data frame
  names(engFeatures) <- c( # give meaningful names
    paste("Variance_Window",unlist(varFeat_rol_window),"lagged", varFeat_rol_lag, sep  ="_"),
    paste("mean_Window",unlist(meFeat_rol_window),"lagged", meFeat_rol_lag, sep  ="_"),
    paste("lagged", lagFeat_lag, sep  ="_"))
  return(engFeatures)
}

EngineeredFeatures <- foreach(i = 3:ncol(rawData), .combine = cbind) %do% {
  feat <- names(rawData)[i]
  newFeat <- ENG(rawData[ ,i], rawData$Y)
  label <- names(newFeat)
  foreach(n = 1:length(names(newFeat)), .combine = c) %do% {
    label[n] <- paste(feat,label[n], sep = "_")
    
  }
  names(newFeat) <- label
  newFeat
  
}

EngineeredFeatures <- EngineeredFeatures[ ,colSums(is.na(EngineeredFeatures))<nrow(EngineeredFeatures)]

Correlation <- apply(EngineeredFeatures, 2, cor, y = rawData$Y, use = "complete.obs")
`# NA's` <- colSums(is.na(EngineeredFeatures))

plot(x = `# NA's`, y = 1 - abs(Correlation))

EngineeredFeatures <- EngineeredFeatures[ ,colSums(is.na(EngineeredFeatures))<22]


modelData <- c(rawData, EngineeredFeatures)
modelData <- as.data.frame(modelData)
modelData <- modelData[complete.cases(modelData), ]
results <- data.frame(Date =modelData[year(modelData$Date) > 2018, 10])
trainAss <- data.frame(Date = modelData[year(modelData$Date) <= 2018, 10])

testData <- modelData[year(modelData$Date) > 2018, -1]
trainData <- modelData[year(modelData$Date) <= 2018, -1]

mean <- apply(trainData[ , -1], 2, mean)
std <- apply(trainData[ , -1], 2, sd)

testPred = scale(testData[ ,-1], center = mean, scale = std)
testResponse = testData[,-1]

results$Actual <- testResponse

trainPred = scale(trainData[ ,-1], center = mean, scale = std)
trainResponse = trainData[-1] 

trainAss$Actual <- trainResponse

MOD <- earth(x = trainPred, y = trainResponse, pmethod = "cv", ncross = 3, nfold = 5)
summary(MOD)
results$Predictions <- predict(MOD, newdata = testPred)
trainAss$Predictions <- predict(MOD)
plot(MOD)

ggplot(data = results) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Actual), colour = "blue") 
ggplot(data = trainAss) + geom_line(aes(x = Date, y = Predictions), colour = "red") +geom_line(aes(x = Date, y = Actual), colour = "blue") 




