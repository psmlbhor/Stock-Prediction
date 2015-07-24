library(forecast)
library(fpp)


stlplot <- function(choice){
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
      
      #plot(tsStock,lw=2,col="blue",xlim=c(2000,2013))
      #lines(tsStocktrend1,lw=2,col="red")
      #this abline function plots lines in the currently plotted graph
      #the v argument states where to draw the dotted vertical line
      #lty is line type, here lty=3 means a dotted line
      #abline(v=2015.5,lty=3)
      
      #Decompose a time series into seasonal, trend and irregular components based on loess method
      # get second generalized "trend" function (TREND = tsStocktrend2)
      stlStock = stl(tsStock,s.window = "periodic")
      #plot(stlStock,col="blue",lw=2)
      tsStocktrend2 = stlStock$time.series[,2]
      #plot(forecast(stlStock))
      #abline(v=2015.5,lty = 3)
      
      #plot(tsStock,lw=3)
      #lines(tsStocktrend1,col="purple",lw=2)
      #lines(tsStocktrend2,col="red",lw=2)
      #abline(v=2015.5,lty=3)
      #legend("bottomleft",legend=c("Actual Function","STL trend","Polynomial Trend"),col=c("black","red","purple"),lw=2)
      
      
      #start predicting #
      #based on polynomial function#
      HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
      HWStock1 = HoltWinters(tsStocktrend1)
      NETfit1 <- nnetar(tsStocktrend1)
      autofit1 = auto.arima(tsStocktrend1)
      #fit12 <- arima(tsStocktrend1,order=c(1,0,0),seasonal = list(order=c(2,1,0),period=12),method = "ML",optim.method="Nelder-Mead")
      fit11 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
      stlStock1 = stl(tsStocktrend1,s.window="periodic")
      
      
      
      #Based on STL function #
      
      HWStock2_ng = HoltWinters(tsStocktrend2,gamma=FALSE)
      HWStock2 = HoltWinters(tsStocktrend2)
      NETfit2 <- nnetar(tsStocktrend2)
      autofit2 = auto.arima(tsStocktrend2)
      #fit2 <- Arima(tsStocktrend2, order=c(15,3,3))
      #fit22 <- arima(tsStocktrend2, order=c(1,0,0),list(order=c(2,1,0),period=12))
      fit12 <- tslm(tsStocktrend2 ~ trend +season, lambda = 0)
      stlStock2 = stl(tsStocktrend1,s.window="periodic")
      
      tobereturned <- plot(forecast(autofit1,h=24),xlim=c(2008,2017.2),ylim=c(min_value-30,max_value+50),lw=2,col="red",xlab="Time",ylab="Stock Price",main="Predictions of the STL trend")
      
      lines(tsStock,lw=3)
      lines(forecast(stlStock2,h=24)$mean,col="red",lw=2)
      #lines(forecast(fit2,h=24)$mean,lw=2,col="purple")
      lines(tsStocktrend2,lw=2,col="red")
      lines(forecast(NETfit2,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStock2,n.ahead=24),lw=2,col="green")
      lines(predict(HWStock2_ng,n.ahead=24),lw=2,col="green")
      lines(predict(HWStock2,n.ahead=24,prediction.interval=T,level=0.95)[,2],col="orange")
      lines(predict(HWStock2,n.ahead=24,prediction.interval=T,level=0.95)[,3],col="orange")
      legend("bottomleft",legend=c("Actual Function","STL Trend","Predicion - Holt Winters","Prediction - Arima(auto)","Prediciton - Neural Nets","Prediction - Linear Model"),col=c("black","red","green","blue","purple","brown","orange"),lw=2,cex=0.75)
      
      
      
      abline(v=2015.5,lty=3)
      
      return(tobereturned)
}
