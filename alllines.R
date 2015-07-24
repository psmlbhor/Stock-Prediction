library(forecast)
library(fpp)


alllines <- function(choice){
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
      t1 = seq(2009,2015,length=length(tsStock))
      t12 = t1^7
      polyStock = lm(tsStock ~ t1 + t12)
      tsStocktrend1 = ts(polyStock$fit,start=c(2008,1),frequency=12)
      
      #Decompose a time series into seasonal, trend and irregular components based on loess method
      # get second generalized "trend" function (TREND = tsStocktrend2)
      stlStock = stl(tsStock,s.window = "periodic")
      tsStocktrend2 = stlStock$time.series[,2]
      
      #start predicting #
      #based on polynomial function#
      HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
      HWStock1 = HoltWinters(tsStocktrend1)
      NETfit1 <- nnetar(tsStocktrend1)
      autofit1 = auto.arima(tsStocktrend1)
      #fit12 <- arima(tsStocktrend1,order=c(1,0,0),list(order=c(2,1,0),period=12),optim.method="Nelder-Mead")
      fit11 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
      stlStock1 = stl(tsStocktrend1,s.window="periodic")
      
      HWStock2_ng = HoltWinters(tsStocktrend2,gamma=FALSE)
      HWStock2 = HoltWinters(tsStocktrend2)
      NETfit2 <- nnetar(tsStocktrend2)
      autofit2 = auto.arima(tsStocktrend2)
      #fit2 <- Arima(tsStocktrend2, order=c(15,3,3))
      #fit22 <- arima(tsStocktrend2, order=c(1,0,0),list(order=c(2,1,0),period=12))
      fit12 <- tslm(tsStocktrend2 ~ trend +season, lambda = 0)
      stlStock2 = stl(tsStocktrend1,s.window="periodic")
      
      
      HWStockr_ng = HoltWinters(tsStock,gamma=FALSE)
      HWStockr = HoltWinters(tsStock)
      NETfitr <- nnetar(tsStock)
      autofitr = auto.arima(tsStock)
      #fitr <- Arima(tsStock, order = c(15,3,3))
      #fitr2 <- arima(tsStock,order=c(1,0,0),list(order=c(2,1,0),period=12))
      fitlr <- tslm(tsStock ~ trend + season, lambda = 0)
      stlStockr = stl(tsStock,s.window="periodic")
      
      
      tobereturned <- plot(forecast(autofitr,h=24),lw=2,xlim=c(2008,2017.2),ylim=c(min_value-30,max_value+50),col="blue",xlab="Time",ylab="Stock Price",main="All 22 predictions competing")
      #lines(forecast(fitr,h=24)$mean,lw=2,col="purple")
      #lines(forecast(fitr2,h=24)$mean,lw=2,col="purple")
      lines(tsStock,lw=3)
      lines(forecast(NETfitr,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStockr,n.ahead=24),lw=2,col="green")
      lines(predict(HWStockr_ng,n.ahead=24),lw=2,col="green")
      lines(forecast(autofit2,h=24)$mean,lw=2,col="blue")
      #lines(forecast(fit12,h=24)$mean,lw=2,col="purple")
      lines(tsStock,lw=3)
      lines(forecast(stlStock1,h=24)$mean,col="yellow",lw=3)
      lines(forecast(stlStock2,h=24)$mean,col="yellow",lw=3)
      lines(forecast(stlStockr,h=24)$mean,col="yellow",lw=3)
      #lines(forecast(fit2,h=24)$mean,lw=2,col="purple")
      #lines(forecast(fit22,h=24)$mean,lw=2,col="purple")
      lines(tsStocktrend2,lw=2,col="red")
      lines(tsStocktrend1,lw=2,col="red")
      lines(forecast(NETfit2,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStock2,n.ahead=24),lw=2,col="green")
      lines(predict(HWStock2_ng,n.ahead=24),lw=2,col="green")
      lines(forecast(autofit1,h=24)$mean,lw=2,col="blue")
      lines(forecast(fitlr,h=24)$mean,lw=2,col="orange")
      lines(forecast(fit11,h=24)$mean,lw=2,col="orange")
      lines(forecast(fit12,h=24)$mean,lw=2,col="orange")
      lines(tsStock,lw=3)
      lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")
      lines(predict(HWStock1,n.ahead=24,prediction.interval=T,level=0.95)[,1],lw=2,col="green")
      lines(predict(HWStock1,n.ahead=24,prediction.interval=T,level=0.95)[,2],lw=2,col="green")
      lines(predict(HWStock1,n.ahead=24,prediction.interval=T,level=0.95)[,3],lw=2,col="green")
      legend("bottomleft",legend=c("Actual function","prediction - holt winters","prediction - arima(auto)","prediction - neural nets","prediction - linear model"),col=c("black","green","blue","purple","brown","orange"),lw=2,cex=0.75)
      abline(v=2015.5,lty=3)
      
      return(tobereturned)
      
}
      
