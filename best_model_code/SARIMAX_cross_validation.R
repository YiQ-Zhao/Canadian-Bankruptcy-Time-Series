# use different validation set and get the best model

library(data.table)
library(dplyr)
for (valyear in 1999:2009){
  valindex <- 288-((2010-valyear)+1)*12 +1
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
  
  RMSE <- list()
  # 1
  pred <- tryCatch({arima(train_bankr, order=c(3,0,0), seasonal = list(order=c(2,1,3)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['3 0 0 2 1 3 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 2
  pred <- tryCatch({arima(train_bankr, order=c(1,0,3), seasonal = list(order=c(3,1,1)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['1 0 3 3 1 1 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 3
  pred <- tryCatch({arima(train_bankr, order=c(3,0,0), seasonal = list(order=c(3,1,3)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['3 0 0 3 1 3 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 4
  pred <- tryCatch({arima(train_bankr, order=c(2,0,0), seasonal = list(order=c(1,1,3)), xreg = traindf_unemp_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['2 0 0 1 1 3 unemp_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 5
  pred <- tryCatch({arima(train_bankr, order=c(1,0,3), seasonal = list(order=c(1,1,3)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['1 0 3 1 1 3 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 6 
  pred <- tryCatch({arima(train_bankr, order=c(3,0,0), seasonal = list(order=c(2,1,3)), xreg = traindf_pop_house) %>% 
    forecast(h = 24, xreg=testdf_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['3 0 0 2 1 3 pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  # 7
  pred <- tryCatch({arima(train_bankr, order=c(3,0,0), seasonal = list(order=c(3,1,1)), xreg = traindf_unemp_pop_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['3 0 0 3 1 1 unemp_pop_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  #8
  pred <- tryCatch({arima(train_bankr, order=c(1,0,0), seasonal = list(order=c(2,1,1)), xreg = traindf_unemp_house) %>% 
    forecast(h = 24, xreg=testdf_unemp_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['1 0 0 2 1 1 unemp_house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  #9
  pred <- tryCatch({arima(train_bankr, order=c(1,0,3), seasonal = list(order=c(3,1,3)), xreg = traindf_unemp_pop) %>% 
    forecast(h = 24, xreg=testdf_unemp_pop)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['1 0 3 3 1 3 unemp_pop']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  #10
  pred <- tryCatch({arima(train_bankr, order=c(2,0,0), seasonal = list(order=c(3,1,3)), xreg = traindf_house) %>% 
      forecast(h = 24, xreg=testdf_house)}, error = function(cond) {return(NA)})
  if (!is.na(pred)) {RMSE[['2 0 0 3 1 3 house']] <- sqrt(mean((pred[['mean']] - test_bankr)^2))}
  
  
  save(RMSE, file = paste(valyear, valyear+1, '.RData'))
  }

# 09-10
load("/Users/feiran/Desktop/USF/MSAN604/project/2009 2010 .RData")
cv <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv, keep.rownames = TRUE)

# 08-09
load("/Users/feiran/Desktop/USF/MSAN604/project/2008 2009 .RData")
cv08 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv08, keep.rownames = TRUE)
cv <- left_join(cv, cv08, by='rn')

# 07-08
load("/Users/feiran/Desktop/USF/MSAN604/project/2007 2008 .RData")
cv07 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv07, keep.rownames = TRUE)
cv <- left_join(cv, cv07, by='rn')

# 06-07
load("/Users/feiran/Desktop/USF/MSAN604/project/2006 2007 .RData")
cv06 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv06, keep.rownames = TRUE)
cv <- left_join(cv, cv06, by='rn')

# 05-06
load("/Users/feiran/Desktop/USF/MSAN604/project/2005 2006 .RData")
cv05 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv05, keep.rownames = TRUE)
cv <- left_join(cv, cv05, by='rn')

# 04-05
load("/Users/feiran/Desktop/USF/MSAN604/project/2004 2005 .RData")
cv04 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv04, keep.rownames = TRUE)
cv <- left_join(cv, cv04, by='rn')

# 03-04
load("/Users/feiran/Desktop/USF/MSAN604/project/2003 2004 .RData")
cv03 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv03, keep.rownames = TRUE)
cv <- left_join(cv, cv03, by='rn')

# 02-03
load("/Users/feiran/Desktop/USF/MSAN604/project/2002 2003 .RData")
cv02 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv02, keep.rownames = TRUE)
cv <- left_join(cv, cv02, by='rn')

# 01-02
load("/Users/feiran/Desktop/USF/MSAN604/project/2001 2002 .RData")
cv01 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv01, keep.rownames = TRUE)
cv <- left_join(cv, cv01, by='rn')

# 00-01
load("/Users/feiran/Desktop/USF/MSAN604/project/2000 2001 .RData")
cv00 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv00, keep.rownames = TRUE)
cv <- left_join(cv, cv00, by='rn')

# 99-00
load("/Users/feiran/Desktop/USF/MSAN604/project/1999 2000 .RData")
cv99 <- as.data.frame(t(as.data.frame(RMSE)))
setDT(cv99, keep.rownames = TRUE)
cv <- left_join(cv, cv99, by='rn')

avgcv <- data.frame(id=cv[,1], avgcv=rowMeans(cv[,-1]))
# X3.0.0.3.1.1.unemp_pop_house is the best one
