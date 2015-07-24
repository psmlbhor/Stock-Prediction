library(forecast)
library(fpp)


actualplot <- function(choice){
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
      
      
      
      #based on actual function #
      HWStockr_ng = HoltWinters(tsStock,gamma=FALSE)
      HWStockr = HoltWinters(tsStock)
      NETfitr <- nnetar(tsStock)
      autofitr = auto.arima(tsStock)
      #fitr <- Arima(tsStock, order = c(15,3,3))
      #fitr2 <- arima(tsStock,order=c(1,0,0),list(order=c(2,1,0),period=12))
      fitlr <- tslm(tsStock ~ trend + season, lambda = 0)
      stlStockr = stl(tsStock,s.window="periodic")
      
      tobereturned <- plot(forecast(autofitr,h=24),xlim=c(2008,2017.2),ylim=c(min_value-30,max_value+50),lw=2,col="blue",xlab="Time",ylab="Stock Price",main="Predictions of the actual model")
      
      lines(forecast(fitlr,h=24)$mean,col="orange")
      lines(forecast(stlStockr,h=24)$mean,col="red",lw=2)
      #lines(forecast(fitr,h=24)$mean,lw=2,col="purple")
      #lines(forecast(fitr2,h=24)$mean,lw=2,col="purple")
      lines(tsStock,lw=3)
      lines(forecast(NETfitr,h=24)$mean,lw=3,lty="longdash",col="brown");
      lines(predict(HWStockr,n.ahead=24),lw=2,col="green")
      lines(predict(HWStockr_ng,n.ahead=24),lw=2,col="green")
      abline(v=2015.5,lty=3)
      legend("bottomleft",legend=c("Actual function","prediction - holt winters","prediction - arima(auto)","prediction - neural nets","prediction - linear model"),col=c("black","green","blue","purple","brown","orange"),lw=2,cex=0.75)

      return(tobereturned)      
}