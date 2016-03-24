library(forecast)
library(fpp)


polynomialplot <- function(choice){
  
  
      #reading in data
      #please replace the above address with yours
      ##########################################################################################
      Stock = read.table(paste("/home/ubuntu/ShinyApps/spredict/stocks/",choice,".csv",sep = ""),sep = ",",header = TRUE)
      
      #converting to month is not needed when you download monthly data
      #Stock$Date = as.Date(Stock$Date)
      #Stock_monthly = agregate(Stock$close,by = list(Date=format(Stock$Date,"%Y%m")),mean)
      
      #converting to ts
      tryCatch({ tsStock = ts(Stock$Close,start=c(2008,1),end=c(2015,6),frequency=12)})
      
      max_value = max(tsStock)
      min_value = min(tsStock)
      
      #Generalize function as Polynomial "trend" (TREND = toStocktrend1)
      t1 = seq(2008,2015,length=length(tsStock))
      t12 = t1^7
      polyStock = lm(tsStock ~ t1 + t12)
      tsStocktrend1 = ts(polyStock$fit,start=c(2008,1),frequency=12)
      
      
      #Decompose a time series into seasonal, trend and irregular components based on loess method
      # get second generalized "trend" function (TREND = tsStocktrend2)
      stlStock = stl(tsStock,s.window = "periodic")
      plot(stlStock,col="blue",lw=2)
      tsStocktrend2 = stlStock$time.series[,2]
      plot(forecast(stlStock))
      abline(v=2015.5,lty = 3)
      
      
      
      #start predicting #
      #based on polynomial function#
      HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
      HWStock1 = HoltWinters(tsStocktrend1)
      NETfit1 <- nnetar(tsStocktrend1)
      autofit1 = auto.arima(tsStocktrend1)
      #fit12 <- arima(tsStocktrend1,order=c(1,0,0),list(order=c(2,1,0),period=12),optim.method="Nelder-Mead")
      fit11 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
      stlStock1 = stl(tsStocktrend1,s.window="periodic")
      
      tobereturned <- plot(forecast(autofit1,h=24),xlim=c(2008,2017.2),ylim=c(min_value-30,max_value+50),lw=2,col="red",xlab="Time",ylab="Stock Price",main="Predictions of the polynomial trend")
      
      lines(forecast(stlStock1,h=24)$mean,col="red",lw=2)
      lines(tsStock,lw=3)
      lines(forecast(fit11,h=24)$mean,col="orange")
      lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")
      #lines(forecast(fit12,h=24)$mean, lw=2,col="purple")
      lines(predict(HWStock1,n.ahead=24,prediction.interval = T, level = 0.95)[,1],lw=2,col="green")
      
      lines(predict(HWStock1,n.ahead = 24,prediction.interval=T,level = 0.95)[,2],col="green")
      
      lines(predict(HWStock1,n.ahead = 24,prediction.interval = T, level = 0.95)[,3],col="green")
      
      legend("bottomleft",legend=c("Actual Function","Polynomial Trend","Prediction - Holt Winters","Prediction - Arima (auto)","Prediction - Neural Nets","Prediction - Linear Model"),col=c("black","red","green","blue","purple","brown","orange"),lw=1,cex=0.75)
      
      
      abline(v = 2015.5,lty=3)
      
      return(tobereturned)
}