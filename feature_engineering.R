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

## Import Data change date into a meaningful format
file <- "AUS_Data.xlsx" # set source data file name
dataIn <-  read_excel(file, # import data from .xlsx
                      col_name = TRUE, # first row is column names
                      col_types = rep(c("numeric"), times = 9)) # import all variables as numeric

rawData <- data.frame(dataIn[-1 , ]) # change data structure to df drop the first row as it was the column names.

rawData[ ,1] <- as.Date(as.POSIXct(# convert excel time to POSIXct
  rawData[ ,1]*24*60*60, #excel time is days since 1990, POSIXct expects seconds from origin
  origin = "1900-01-01"))  

colnames(rawData)[1] <- c("Date") # set column name

## Input missing values
rawData$X6 <- impute_AR1_Gaussian(rawData$X6) # values are in the middle and no clear trend so we will use Gaussian mixture
attr(rawData$X6, "index_miss") <- NULL # remove attribute from data frame for simplicity

X7_NA <- lm(X7 ~ Date, # values are at the end and part of a stable linear trend so we will impute with linear extrapolation
            data = rawData[year(rawData$Date) > 2010 & !is.na(rawData$X7), ]) # fit linear model on data after 2010 and not missing
for(i in 1:nrow(rawData)){ # cycle through missing values and input using linear model
  if(is.na(rawData$X7[i])){
    rawData$X7[i] = predict.lm(X7_NA, newdata = data.frame(Date = rawData$Date[i]))
  }
}

## Remove trends and correct for GST

rawData$T1 <- insert(diff(rawData$X6), 1, values = NA) / rawData$X6 * 100 # create T1 variable % change period on period to X6

rawData <- rawData %>% mutate(T2 = X6 / X7 * 100) %>% # create T2 Variable X6 normalized for population growth
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
  else{ # if not meaningful return NA's for all values
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
  else{ # if not meaningful return NA's for all values
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
  else{ # if not meaningful return NA's for all values
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
  
   n <- ifelse(length(which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)]))) == 0, # determine position of most correlated feature, check its non zero
               1, # if doesnt exist set to 1 and NA's will carry to next step
          which.max(unlist(varFeatures[seq(from = 2, to = length(varFeatures), by = 3)]))*3-2) # other wise set position (offset 2 position to return corresponding features)
  
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

engineeredFeatures <- foreach(i = 2:ncol(rawData), .combine = cbind) %do% { # apply ENG function to each column (except date) of the raw data including Y to address auto correlation
  feat <- names(rawData)[i] # name of the feature currently loaded
  newFeat <- ENG(rawData[ ,i], rawData$Y) # list of new features
  label <- names(newFeat) # new feature names
  foreach(n = 1:length(names(newFeat)), .combine = c) %do% { # cycle over new feature names and add the name of the original feature
    label[n] <- paste(feat,label[n], sep = "_")
  }
  names(newFeat) <- label # assign new names
  newFeat # export list
}

## Inspect new feature space and evaluate which features should be kept
engineeredFeatures <- engineeredFeatures[ ,colSums(is.na(engineeredFeatures))<nrow(engineeredFeatures)] # inspect each column, if all NA's remove

Correlation <- apply(engineeredFeatures, 2, cor, y = rawData$Y, use = "complete.obs") # calculate correlation between each feature and Y
`# NA's` <- colSums(is.na(engineeredFeatures)) # count the number of NA's (ie how many observations will be lost by retaining a certain feature)

plot(x = `# NA's`, y = 1 - abs(Correlation)) # plot # NA's vs inverse correlation to determine if good value
text(x = `# NA's`, y = .95 - abs(Correlation), label = `# NA's`) # add labels for # NA's for easy identification
engineeredFeatures <- engineeredFeatures[ ,colSums(is.na(engineeredFeatures))<22] # Remove features with more than 21 NA's

modelData <- data.frame(rawData, engineeredFeatures) # combine original variables and engineered features into data frame
modelData <- modelData[complete.cases(modelData), ] # remove observations which are incomplete ( due to rolling window at beginning)

## Inspect for correlation 

dataMatrix <- as.matrix(modelData[ , -1]) # convert to matrix, excluding date

pCor <- rcorr(dataMatrix, type = "pearson") # return pearson correlation and signifigance levels
pCor_matrix <- pCor$r # correlation matrix
pCor_sig <- pCor$P # signifigance levels

toRemove <- foreach(m = 2:ncol(pCor_matrix),  .combine = c) %do% { #remove junk (defined as correlation with another feature > 0.95 or significance of correlation with Y < .05)
  foreach(n = 2:nrow(pCor_matrix), .combine = c) %do% { # iterate over all rows and columns
    if (n != m){ # if not on self
      if(pCor_matrix[n,m] > 0.95 | pCor_sig[1,m] > 0.05){ # test for "junk"
        colName <- rownames(pCor_matrix)[m] #find variable name
        colName
      }
    }
  }
}
  
toRemove <- unique(toRemove) # unique names only
(toRemove) # check
length(toRemove) # check
modelData <-modelData[ ,names(modelData) %nin% toRemove] # remove junk features

## Build test training split, normalise data.

testData <- modelData[modelData$Date > as.Date("2018-02-28"), c(-1)] # test Data all observations after 28 Feb 2018 (ie March 2018 onwards) removing Date as it will not be used
trainData <- modelData[year(modelData$Date) <= as.Date("2018-02-28"), c(-1)] # train Data all other obs
results <- modelData[modelData$Date > as.Date("2018-02-28"), c(1,2)] # test response and date (for plotting later)
trainAss <- modelData[year(modelData$Date) <= as.Date("2018-02-28"), c(1,2)] # training response and date 

mean <- apply(trainData[ , -1], 2, mean) # calculate mean for each variable
std <- apply(trainData[ , -1], 2, sd) # calculate SD for each variable

testPred = scale(testData[ ,-1], center = mean, scale = std) # scale test data and make predictor df
testResponse = testData[,1] # make response list

trainPred = scale(trainData[ ,-1], center = mean, scale = std) # scale training data and make pred df 
trainResponse = trainData[, 1] # make train response df

## Save objects for use in various models
saveRDS(list(rawData, modelData, testData, trainData, results, trainAss, testPred, testResponse, trainPred, trainResponse, mean, std), "data.RDS")





