# Canadian Bankruptcy Rate Forecasting

**Author: [Kyna Ji](https://github.com/feiran-kyna-ji), [Yue Lan](https://github.com/ylan7), [Akshay Tiwari](https://github.com/akkiittiwari), [Yiqiang Zhao](https://github.com/YiQ-Zhao)**  

## Description
In this report, based on monthly historical statistics of bankruptcy rate, as well as other related factors including unemployment rate, population, and house price index from 1987 to 2010, we forecasted bankruptcy rate in 2011 and 2012. 

## Data
There are 24-year time series of 4 variables in the dataset.   
<p align="center">
<img src="/plots/data2.png" width="700">
</p>

## Models

We tried variety of modeling approaches including:
* Univariate Time Series Model
  - ARIMA/SARIMA (Box-Jenkins Approach)
  - Exponential Smoothing (Holt-Winters Approach)
* Time series models
  - ARIMAX/SARIMAX
  - VAR/VARX (Vector Autoregression)
* Linear regression model

Since our purpose is to predict the bankruptcy rate in the next two years, we would like to maximize the prediction accuracy, which is equivalent to minimizing the prediction errors. The evaluation metric we use is RMSE (root of mean squared errors).  

To avoid overfitting, we adopt the cross-validation technique which takes several different subset of data as validation sets and evaluate the models by their average scores. The training sets and validation sets used are shown as follows.  
<p align="center">
<img src="/plots/cross_validation.png" width="400">
</p>

The cross-validation RMSEs for each model are shown as follows.  
<p align="center">
<img src="/plots/model_rmse.png" width="400">
</p>


Based on the above shown RMSEs, we selected SARIMAX (3,0,0) (3,1,1)[12] (with feature engineering) as the final model.

## Forecast
We re-trained the SARIMAX model using all data from 1987 to 2010, and got following prediction values for the years 2011 and 2012.  
<p align="center">
<img src="/plots/forecast.png" width="500">
</p>
