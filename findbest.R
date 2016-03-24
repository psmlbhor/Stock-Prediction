# Adding missing libraries e.g for forecast:        install.packages("forecast")

library(forecast)
library(fpp)

tryCatch({  
    findBestPrediction <- function(Stockadd)
    {
      
        Stock = Stockadd
      
        # In case not read correctly:
        backUpStock = read.table("/home/ubuntu/ShinyApps/spredict/stocks/3MINDIA.csv", sep=",", header=TRUE);
      
        # Convert into time series object
        #ts() function convert the given discrete observations into time series objects
        #rev() function reverses the given argument
        #tryCatch() is a function in R
        #This is how to use tryCatch()
        #result = tryCatch({
        #expr
        #}, warning = function(w) {
        #warning-handler-code
        #}, error = function(e) {
        #error-handler-code
        #}, finally = {
        #cleanup-code
        #}
        
        #tsStock is the actual function graph that uper niche wala graph
        tryCatch({   tsStock = ts(rev(Stock$Close),start=c(2000, 1),end = c(2015,6),frequency=12)}, error=function(e) { tsStock = ts(rev(Stock$Close),start=c(2008, 1),end=c(2015,6),frequency=12) })
        
        tryCatch({   tsBackUpStock = ts(rev(backUpStock$Close),start=c(2000, 1),end = c(2015,6),frequency=12)}, error=function(e) { backUpStock = ts(rev(Stock$Close),start=c(2008, 1),end=c(2015,6),frequency=12) })
        
        # Create Train and Test data of the input stock
        #window() function extracts a subset of time series
        tryCatch({   train <- window(tsBackUpStock, end=2014)}, error=function(e) { train = 0 })
        tryCatch({   test <- window(tsBackUpStock, start=2015)}, error=function(e) { test = 0 })
        tryCatch({   Btrain <- window(tsBackUpStock, end=2014)}, error=function(e) { Btrain = 0 })
        tryCatch({   Btest <- window(tsBackUpStock, start=2015)}, error=function(e) { Btest = 0 })
        
        #the train dataset is based on the tsStock, i.e., the original plot of the function
        #not the polynomial one
        tryCatch({   train <- window(tsStock, end=2014)}, error=function(e) { train = 0 })
        tryCatch({   test <- window(tsStock, start=2015)}, error=function(e) { test = 0 })
        #print ("\n")
        #print(train)
        #print(test)
        #print(Btrain)
        #print(Btest)
        #print(test[1])
        #print(Btest[1])
        #print(test[2])
        #print(Btest[2])
        #print(test[3])
        #print(Btest[3])
        #print(length(test))
        #print(length(train))
        #I think this is an useless thing to do, doesn't matter will see later
        if (test[1] == Btest[1] && test[2] == Btest[2] && test[3] == Btest[3]) {train = Btrain }
        
        # Mean Absolute Errors of the 25 predictions are stored here
        
        #Now here a matrix is made of size 25*5 b'coz lenght(test)=4
        tryCatch({ mae = matrix(NA,25,length(test)+1)}, error=function(e) {  mae = matrix(NA,25,10000) })
        
        #a sequence from 2000 to 2013 is made of length 121, I dont know why?
        tryCatch({tl = seq(2000,2015,length=length(train))},error=function(e){tl = seq(2008,2015,length=length(train))})
        #all the values in the sequence are raised to the power of 7, still no idea
        tl2 = tl^7
        
        
        #######################################################################################
        #Now all the 24 calculations are done on the train data
        
        
        #all those calculations that are done on tsStocktrend1 are of polynomial trend
        #all those calculations that are dont on tsStocktrend2 are of STL trend
        
        # cat("01")
        #lm() function is used to create a linear model fit and this function returns a
        #lm class object that contains 4 seperate values which you can come to know if
        #you plot plot(polyStock)
        tryCatch({   polyStock = lm(train ~ tl + tl2)}, error=function(e) { polyStock = 0 })
        # cat("02")
        #this time series just makes a polynomial graph of the original function
        tryCatch({   tsStocktrend1=ts(polyStock$fit,start=c(2000, 1),end = c(2015,6),frequency=12)}, error=function(e) { tsStocktrend1 = 0 })
        # cat("03")
        #calculating the seasonal trend loess, the seasonal change over a period of one year
        tryCatch({  stlStock = stl(train,s.window="periodic")}, error=function(e) { stlStock = 0 })
        # cat("04")
        #stl() returns an object of class stl with time.series as component which contains
        #seasonal, trend and remainder as its columns. We extract second column
        tryCatch({   tsStocktrend2 = stlStock$time.series[,2]}, error=function(e) { tsStocktrend2 = 0 })
        
        
        
        
        # cat("05")
        #holtwinters filtering is used to smoothen the curve and takes into account level, trend and
        #seasonal components of a time series. The gamma = FALSE means that non-seasonal model is to be fitted
        tryCatch({   HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)}, error=function(e) { HWStock1_ng = 0 })
        # cat("06")
        #here gamma not specified, so seasonal model is to be fitted
        tryCatch({   HWStock1 = HoltWinters(tsStocktrend1)}, error=function(e) { HWStock1 = 0 })
        # cat("07")
        #neural network is a very complicated concept and I really didn't understood it well
        #but I got to know that nnetar is a function in the forecast package and returns an object 
        #of class nnetar. We can find more about this using summary(NETfit1) and plot it
        #using plot(forecast(NETfit1)). Refer script.r
        tryCatch({   NETfit1 <- nnetar(tsStocktrend1)}, error=function(e) { NETfit1 = 0 })
        # cat("08")
        tryCatch({   autofit1 =  auto.arima(tsStocktrend1)}, error=function(e) { autofit1 = 0 })
        # cat("09") 
          
        #tryCatch({   fit12 <-   arima(tsStocktrend1, order=c(1,0,0), list(order=c(2,1,0), period=12))}, error=function(e) { fit12 = 0 })
        # cat("010")
        #tslm is used to fit linear models to time series including trend and seasonality components. 
        #tslm is largely a wrapper for lm() except that it allows variables "trend" and "season" which 
        #are created on the fly from the time series characteristics of the data.
        tryCatch({   fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)}, error=function(e) { fitl1 = 0 })
        # cat("011")
        tryCatch({   stlStock1 = stl(tsStocktrend1,s.window="periodic")}, error=function(e) { stlStock1 = 0 })
        
        
        
        # cat("012")
        #from here, all the above operations are being done on the stl trend function 
        tryCatch({  HWStock2_ng = HoltWinters(tsStocktrend2,gamma=FALSE)}, error=function(e) { HWStock2_ng = 0 })
        # cat("013") 
        tryCatch({   HWStock2 = HoltWinters(tsStocktrend2)}, error=function(e) { HWStock2 = 0 })
        # cat("014")
        tryCatch({   NETfit2 <- nnetar(tsStocktrend2)}, error=function(e) { NETfit2 = 0 })
        # cat("015") 
        tryCatch({   autofit2 =  auto.arima(tsStocktrend2) }, error=function(e) { autofit2 = 0 })
        # cat("016")
        #tryCatch({   fit2 <-    arima(tsStocktrend2, order=c(15,3,3))}, error=function(e) { fit2 = 0 })
        # cat("017")
        #tryCatch({   fit22 <-    arima(tsStocktrend2, order=c(1,0,0), list(order=c(2,1,0), period=12))}, error=function(e) { fit22 = 0 })
        # cat("018") 
        tryCatch({   fitl2 <- tslm(tsStocktrend2 ~ trend + season, lambda=0)}, error=function(e) { fitl2 = 0 })
        # cat("019") 
        tryCatch({   stlStock2 = stl(tsStocktrend1,s.window="periodic")}, error=function(e) { stlStock2 = 0 })
       
        
        
        # cat("020")
        #here all the above operations are being performed on the train data set directly
        #I think, the original function is being worked upon here now
        tryCatch({   HWStockr_ng = HoltWinters(train,gamma=FALSE)}, error=function(e) { HWStockr_ng = 0 })
        # cat("021")
        tryCatch({   HWStockr = HoltWinters(train)}, error=function(e) { HWStockr = 0 })
        # cat("022") 
        tryCatch({   NETfitr <- nnetar(train)}, error=function(e) { NETfitr = 0 })
        # cat("023") 
        tryCatch({   autofitr = auto.arima(train)}, error=function(e) { autofitr = 0 })
        # cat("024")
        #tryCatch({   fitr <-arima(train, order=c(15,3,3))}, error=function(e) { fitr = 0 })
        # cat("025") 
        #tryCatch({   fitr2 <-  arima(train, order=c(1,0,0), list(order=c(2,1,0), period=12))}, error=function(e) { fitr2 = 0 })
        # cat("026")
        tryCatch({   fitlr <- tslm(train ~ trend + season, lambda=0)}, error=function(e) { fitlr = 0 })
        # cat("027")
        tryCatch({   stlStockr = stl(train,s.window="periodic")} , error=function(e) {stlStockr = 0 })
        
        
        
        
        
        # cat("  	TRANSITION		-!!!-")
        
        
        
        
        
        #These calculations are performed to predict the future data
        #Notice that the start of the time series functions is the year 2013
        # cat("1")
        tryCatch({   HWStockr_ng = HoltWinters(train,gamma=FALSE)}, error=function(e) { HWStockr_ng = 0 })
        # cat("2")
        tryCatch({  predautofitr =   window(forecast(autofitr,h=39)$mean, start=2015)}, error=function(e) { predautofitr = 0 })
        # cat("3")
        
        #forecast() is used to forecast the future values of a given time series
        #first argument is the time series object and the second h=39 is the number of periods for forecasting
        #here it refers to the prediction for 39 months
        #tryCatch({  predfitr =  window(forecast(fitr,h=39)$mean, start=2013)}, error=function(e) { predfitr = 0 })
        # cat("4")
        #tryCatch({  predfitr2  =   window(forecast(fitr2,h=39)$mean, start=2013)}, error=function(e) { predfitr2 = 0 })
        # cat("5")
        tryCatch({  predNETfitr =  window(forecast(NETfitr,h=39)$mean, start=2015)}, error=function(e) { predNETfitr = 0 })
        # cat("6")
        tryCatch({  predHWStockr =  window(predict(HWStockr,n.ahead=39), start=2015)}, error=function(e) { predHWStockr = 0 })
        # cat("7")
        tryCatch({   predHWStockr_ng =  window(predict(HWStockr_ng,n.ahead=39), start=2015)}, error=function(e) { predHWStockr_ng  = 0 })
        # cat("8")
        tryCatch({ predautofit2 =   window(forecast(autofit2,h=39)$mean, start=2015)}, error=function(e) { predautofit2 =0 })
        # cat("9")
        #tryCatch({  predfit12 =  window(forecast(fit12,h=39)$mean, start=2015)}, error=function(e) { predfit12 = 0 })
        # cat("10")
        #tryCatch({   predfit2 = window(forecast(fit2,h=39)$mean, start=2015)}, error=function(e) { predfit2 = 0 })
        # cat("11")
        #tryCatch({ predfit22 =  window(forecast(fit22,h=39)$mean, start=2015)}, error=function(e) { predfit22 = 0 }) # cat("C2")}, error=function(e) { predfit22 = 0 })
        # cat("12")
        tryCatch({   predstlStock1 = window( forecast(stlStock1, h=39)$mean, start=2015)}, error=function(e) { predstlStock1 = 0 })
        # cat("13")
        tryCatch({   predstlStock2 =  window(forecast(stlStock2, h=39)$mean, start=2015)}, error=function(e) { predstlStock2 = 0 })
        # cat("14")
        tryCatch({   predstlStockr =  window(forecast(stlStockr, h=39)$mean, start=2015)}, error=function(e) { predstlStockr = 0 })
        # cat("15")
        tryCatch({   predNETfit2 =  window(forecast(NETfit2,h=39)$mean, start=2015)}, error=function(e) { predNETfit2 = 0 })
        tryCatch({   predHWStock2 =  window(predict(HWStock2,n.ahead=39), start=2015)}, error=function(e) { predHWStock2 = 0 })
        tryCatch({   predHWStock2_ng =  window(predict(HWStock2_ng,n.ahead=39), start=2015)}, error=function(e) { predHWStock2_ng = 0 })
        tryCatch({    predautofit1 =  window(forecast(autofit1,h=39)$mean, start=2015)}, error=function(e) { predautofit1 = 0 })
        # cat("after autofit")
        tryCatch({   predfitlr = window(forecast(fitlr, h=39)$mean , start=2015)}, error=function(e) { predfitlr = 0 })
        tryCatch({   predfitl1 =   window(forecast(fitl1, h=39)$mean, start=2015)}, error=function(e) { predfitl1 = 0 })
        tryCatch({   predfitl2 = window(forecast(fitl2, h=39)$mean , start=2015)}, error=function(e) { predfitl2 = 0 })
        tryCatch({   predNETfit1 =  window(forecast(NETfit1,h=39)$mean, start=2015)}, error=function(e) { predNETfit1 = 0 })
        tryCatch({   predHWStock1_ng =  window(predict(HWStock1_ng,n.ahead=39), start=2015)}, error=function(e) { predHWStock1_ng = 0 })
        #predict function takes the argument as object of the class inheriting linear model(lm)
        #level indicates the tolerance level generally set to 0.95
        #it returns vector with 3 columns, fit = avg(lwr+upr),lwr(lower) and upr(upper)
        #we are accessing all columns here
        tryCatch({   predHWStock11 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,1], start=2015)}, error=function(e) { predHWStock11 = 0 })
        tryCatch({   predHWStock12 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,2], start=2015)}, error=function(e) { predHWStock12 = 0 })
        tryCatch({   predHWStock13 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,3], start=2015)}, error=function(e) { predHWStock13 = 0 })
        # cat("  	NEXT	--!!!--")
        
        # Calculate Mean Absolute Error 
        for(i in 1:length(test))
        {
          tryCatch({  mae[1,i] <- abs <- abs(predautofitr[i]-test[i])  })
          #tryCatch({    mae[2,i] <-abs(predfitr[i]-test[i]) }, error=function(e) { })
          #tryCatch({    mae[3,i] <- abs <- abs(predfitr2[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[4,i] <- abs(predNETfitr[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[5,i] <- abs(predHWStockr[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[6,i] <- abs(predHWStockr_ng[i]-test[i]) }, error=function(e) { })
          tryCatch({  mae[7,i] <- abs(predautofit2[i]-test[i])  })
          #tryCatch({    mae[8,i] <- abs(predfit12[i]-test[i]) }, error=function(e) { })
          #tryCatch({    mae[9,i] <-abs(predfit2[i]-test[i]) }, error=function(e) { })
      	# cat("before mae 22")
         #tryCatch({   mae[10,i] <- abs(predfit22[i]-test[i]) }, error=function(e) { })
          # cat("after mae 22")
         tryCatch({ 	mae[11,i] <- abs(predstlStock1[i]-test[i]) }, error=function(e) { })
         tryCatch({     mae[12,i] <- abs(predstlStock2[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[13,i] <- abs(predstlStockr[i]-test[i]) }, error=function(e) { })
          
          tryCatch({    mae[14,i] <- abs(predNETfit2[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[15,i] <- abs(predHWStock2[i]-test[i]) }, error=function(e) { })
         tryCatch({     mae[16,i] <- abs(predHWStock2_ng[i]-test[i]) }, error=function(e) { })
          
         tryCatch({     mae[17,i] <- abs(predautofit1[i]-test[i]) })
          tryCatch({    mae[18,i] <- abs(predfitlr[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[19,i] <-  abs(predfitl1[i]-test[i]) }, error=function(e) { })
           tryCatch({   mae[20,i] <- abs(predfitl2[i]-test[i]) }, error=function(e) { })
           tryCatch({   mae[21,i] <- abs(predHWStock1_ng[i]-test[i] ) }, error=function(e) { })
          tryCatch({    mae[22,i] <- abs(predNETfit1[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[23,i] <- abs(predHWStock11[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[24,i] <- abs(predHWStock12[i]-test[i]) }, error=function(e) { })
          tryCatch({    mae[25,i] <- abs(predHWStock13[i]-test[i]) }, error=function(e) { })
        }
        
        # Sum all Errors
        for(i in 1:25)
        {
          mae[i,5] = sum(mae[i,1:4])
        }
        
        
        # Find best Prediction
        
        best = which.min(mae[1:25,5])
        min_error = mae[best,5]
        
        
        tobereturned <- c(best,min_error)
        
        #cat("Winning Model ID:", best )
        
        return (tobereturned)
}
    }, error=function(e) { cat("findBestPrediction failed for:",Stockadd); });
