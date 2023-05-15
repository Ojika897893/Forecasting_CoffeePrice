# forecasting_international_priceo_of_coffee
Data provided by the International Coffee Organization (ICO) enabled us to peek the future prices.
  
  I did a forecast using an international price of coffee using several models. For a simple prediction, I concluded that Neural Network performs the best, yet the model does not provide insights to comprehend the data sets. On the other hand, ARIMA(2,1,3)(0,0,1) can explain the characteristics such as unit root, trend, and seasonality. The VAR model is acceptable, but a better result from the Ljung-Box test for Brazil can increase confidence in the model.
  I attempted three transformations to the data to further improve the data to obtain lower out-of-sample RMSE. Simply applying inflation or removing the outliers did not improve the models, yet simultaneous adjustments for inflation and outliers dwarfed the out-of-sample RMSE for ARIMA and VAR. 
