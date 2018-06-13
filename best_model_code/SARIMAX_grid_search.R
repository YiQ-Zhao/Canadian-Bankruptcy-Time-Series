setwd('~/Desktop/USF/MSAN604/project')
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)

valyear <- 2009
valindex <- 265

data <- read.csv('train.csv')
data <- data[1:288,]

train <- data[1:(valindex-1),]
test <- data[valindex:(valindex+23),]

train_unemp <- ts(data = train$Unemployment_Rate, start = c(1987,1), frequency = 12)
train_pop <- ts(data = train$Population, start = c(1987,1), frequency = 12)
train_bankr <- ts(data = train$Bankruptcy_Rate, start = c(1987,1), frequency = 12)
train_house <- ts(data = train$House_Price_Index, start = c(1987,1), frequency = 12)
test_unemp <- ts(data = test$Unemployment_Rate, start = c(valyear,1), frequency = 12)
test_pop <- ts(data = test$Population, start = c(valyear,1), frequency = 12)
test_bankr <- ts(data = test$Bankruptcy_Rate, start = c(valyear,1), frequency = 12)
test_house <- ts(data = test$House_Price_Index, start = c(valyear,1), frequency = 12)

acf(train_bankr,lag.max = 90)
acf(diff(train_bankr),lag.max=90)

traindf_unemp <- data.frame(train_unemp)
testdf_unemp <- data.frame(test_unemp)
traindf_pop <- data.frame(train_pop)
testdf_pop <- data.frame(test_pop)
traindf_house <- data.frame(train_house)
testdf_house <- data.frame(test_house)
traindf_unemp_pop <- data.frame(unemp = train_unemp, pop = train_pop)
testdf_unemp_pop <- data.frame(unemp = test_unemp, pop = test_pop)
traindf_unemp_house <- data.frame(unemp = train_unemp, house = train_house)
testdf_unemp_house <- data.frame(unemp = test_unemp, house = test_house)
traindf_pop_house <- data.frame(pop = train_pop, house = train_house)
testdf_pop_house <- data.frame(pop = test_pop, house = test_house)
traindf_unemp_pop_house <- data.frame(unemp = train_unemp, pop = train_pop, house = train_house)
testdf_unemp_pop_house <- data.frame(unemp = test_unemp, pop = test_pop, house = test_house)

getRMSE <- function(traindf, testdf, filename){
  traindf <- noquote(traindf)
  testdf <- noquote(testdf)
  RMSE <- list()
  for(p in 0:3){
    for(d in 0:1){
      for(q in 0:3){
        for(P in 0:3){
          for(D in 0:1){
            for(Q in 0:3){
              model <- tryCatch({do.call("arima", list(train_bankr, order=c(p,d,q), 
                                                       seasonal = list(order=c(P,D,Q)), 
                                                       xreg = as.name(traindf)))}, 
                error = function(cond) {return(NA)})
#             model <- arima(train_bankr, order=c(p,d,q), seasonal = list(order=c(P,D,Q)), xreg = traindf)
              if (!is.na(model)){
              pred <- do.call("forecast", list(model, h = 24, xreg=as.name(testdf)))
              #pred <- forecast(model, h = 24, xreg=testdf)
              RMSE[[paste(p,d,q,P,D,Q)]] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
              }
          }
        }
      }
    }
  }
  save(RMSE, file = paste(filename,'.RData'))
}

getRMSE('traindf_unemp', 'testdf_unemp', 'unemp')
getRMSE('traindf_pop', 'testdf_pop', 'pop')
getRMSE('traindf_house', 'testdf_house', 'house')
getRMSE('traindf_unemp_pop', 'testdf_unemp_pop', 'unemp_pop')
getRMSE('traindf_unemp_house', 'testdf_unemp_house', 'unemp_house')
getRMSE('traindf_pop_house', 'testdf_pop_house', 'pop_house')
getRMSE('traindf_unemp_pop_house', 'testdf_unemp_pop_house', 'unemp_pop_house')


# save models
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/house .RData")
RMSE_house <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/pop .RData")
RMSE_pop <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/pop_house .RData")
RMSE_pop_house <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/unemp .RData")
RMSE_unemp <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/unemp_house .RData")
RMSE_unemp_house <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/unemp_pop_house .RData")
RMSE_unemp_pop_house <- RMSE
load("/Users/feiran/Desktop/USF/MSAN604/project/2009-2010/unemp_pop .RData")
RMSE_unemp_pop <- RMSE
save(RMSE_house, RMSE_pop, RMSE_pop_house, RMSE_unemp, RMSE_unemp_house, RMSE_unemp_pop_house, RMSE_unemp_pop, file = '09-10.RData')

# get top RMSEs
library(magrittr)
load("/Users/feiran/Desktop/USF/MSAN604/project/09-10.RData")
names(RMSE_house) <- paste(names(RMSE_house), 'house')
names(RMSE_pop) <- paste(names(RMSE_pop), 'pop')
names(RMSE_pop_house) <- paste(names(RMSE_pop_house), 'pop_house')
names(RMSE_unemp) <- paste(names(RMSE_unemp), 'unemp')
names(RMSE_unemp_house) <- paste(names(RMSE_unemp_house), 'unemp_house')
names(RMSE_unemp_pop) <- paste(names(RMSE_unemp_pop), 'unemp_pop')
names(RMSE_unemp_pop_house) <- paste(names(RMSE_unemp_pop_house), 'unemp_pop_house')

RMSE_all <- c(RMSE_house, RMSE_pop, RMSE_pop_house, RMSE_unemp, RMSE_unemp_house, RMSE_unemp_pop_house, RMSE_unemp_pop)
RMSE_all[sort.list(unlist(RMSE_all))][1:50]

pred <- tryCatch({arima(train_bankr, order=c(3,0,0), seasonal = list(order=c(2,1,3)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  RMSE[['3 0 0 2 1 3 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))
