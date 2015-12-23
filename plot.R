library(forecast)
library(fpp)

#reading in data
#please replace the above address with yours
##########################################################################################
Stock = read.table("/home/ubuntu/ShinyApps/spredict/stocks/ABC.csv",sep = ",",header = TRUE)

#converting to month is not needed when you download monthly data
#Stock$Date = as.Date(Stock$Date)
#Stock_monthly = agregate(Stock$close,by = list(Date=format(Stock$Date,"%Y%m")),mean)

#converting to ts
tsStock = ts(Stock$Close,start=c(2000,1),frequency=12)

#Generalize function as Polynomial "trend" (TREND = toStocktrend1)
t1 = seq(2000,2013,length=length(tsStock))
t12 = t1^7
polyStock = lm(tsStock ~ t1 + t12)
tsStocktrend1 = ts(polyStock$fit,start=c(2000,1),frequency=12)

plot(tsStock,lw=2,col="blue",xlim=c(2000,2013))
lines(tsStocktrend1,lw=2,col="red")
#this abline function plots lines in the currently plotted graph
#the v argument states where to draw the dotted vertical line
#lty is line type, here lty=3 means a dotted line
abline(v=2013.25,lty=3)

#Decompose a time series into seasonal, trend and irregular components based on loess method
# get second generalized "trend" function (TREND = tsStocktrend2)
stlStock = stl(tsStock,s.window = "periodic")
plot(stlStock,col="blue",lw=2)
tsStocktrend2 = stlStock$time.series[,2]
plot(forecast(stlStock))
abline(v=2013.25,lty = 3)

plot(tsStock,lw=3)
lines(tsStocktrend1,col="purple",lw=2)
lines(tsStocktrend2,col="red",lw=2)
abline(v=2013.25,lty=3)
legend("bottomleft",legend=c("Actual Function","STL trend","Polynomial Trend"),col=c("black","red","purple"),lw=2)

#start predicting #
#based on polynomial function#
HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
HWStock1 = HoltWinters(tsStocktrend1)
NETfit1 <- nnetar(tsStocktrend1)
autofit1 = auto.arima(tsStocktrend1)
fit12 <- arima(tsStocktrend1,order=c(1,0,0),list(order=c(2,1,0),period=12),optim.method="Nelder-Mead")
fit11 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
stlStock1 = stl(tsStocktrend1,s.window="periodic")

plot(forecast(autofit1,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="red",xlab="Time",ylab="Stock Price",main="Predictions of the polynomial trend")

lines(forecast(stlStock1,h=24)$mean,col="red",lw=2)
lines(tsStock,lw=3)
lines(forecast(fit11,h=24)$mean,col="orange")
lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")
lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")
lines(forecast(fit12,h=24)$mean, lw=2,col="purple")
lines(predict(HWStock1,n.ahead=24,prediction.interval = T, level = 0.95)[,1],lw=2,col="green")

lines(predict(HWStock1,n.ahead = 24,prediction.interval=T,level = 0.95)[,2],col="green")

lines(predict(HWStock1,n.ahead = 24,prediction.interval = T, level = 0.95)[,3],col="green")

legend("bottomleft",legend=c("Actual Function","Polynomial Trend","Prediction - Holt Winters","Prediction - Arima (auto)","Predition - Arima (fixed)","Prediction - Neural Nets","Prediction - Linear Model"),col=c("black","red","green","blue","purple","brown","orange"),lw=1,cex=0.75)


abline(v = 2013.25,lty=3)

#Based on STL function #

HWStock2_ng = HoltWinters(tsStocktrend2,gamma=FALSE)
HWStock2 = HoltWinters(tsStocktrend2)
NETfit2 <- nnetar(tsStocktrend2)
autofit2 = auto.arima(tsStocktrend2)
fit2 <- Arima(tsStocktrend2, order=c(15,3,3))
#fit22 <- arima(tsStocktrend2, order=c(1,0,0),list(order=c(2,1,0),period=12))
fit12 <- tslm(tsStocktrend2 ~ trend +season, lambda = 0)
stlStock2 = stl(tsStocktrend1,s.window="periodic")

plot(forecast(autofit1,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="red",xlab="Time",ylab="Stock Price",main="Predictions of the STL trend")

lines(tsStock,lw=3)
lines(forecast(stlStock2,h=24)$mean,col="red",lw=2)
lines(forecast(fit2,h=24)$mean,lw=2,col="purple")
lines(tsStocktrend2,lw=2,col="red")
lines(forecast(NETfit2,h=24)$mean,lw=3,lty="longdash",col="brown")
lines(predict(HWStock2,n.ahead=24),lw=2,col="green")
lines(predict(HWStock2_ng,n.ahead=24),lw=2,col="green")
lines(predict(HWStock2,n.ahead=24,prediction.interval=T,level=0.95)[,2],col="orange")
lines(predict(HWStock2,n.ahead=24,prediction.interval=T,level=0.95)[,3],col="orange")
legend("bottomleft",legend=c("Actual Function","STL Trend","Predicion - Holt Winters","Prediction - Arima(auto)","Prediction= Arima(fixed)","Prediciton - Neural Nets","Prediction - Linear Model"),col=c("black","red","green","blue","purple","brown","orange"),lw=2,cex=0.75)



abline(v=2013.25,lty=3)


#based on actual function #
HWStockr_ng = HoltWinters(tsStock,gamma=FALSE)
HWStockr = HoltWinters(tsStock)
NETfitr <- nnetar(tsStock)
autofitr = auto.arima(tsStock)
fitr <- Arima(tsStock, order = c(15,3,3))
fitr2 <- arima(tsStock,order=c(1,0,0),list(order=c(2,1,0),period=12))
fitlr <- tslm(tsStock ~ trend + season, lambda = 0)
stlStockr = stl(tsStock,s.window="periodic")

plot(forecast(autofitr,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="blue",xlab="Time",ylab="Stock Price",main="Predictions of the actual model")

lines(forecast(fitlr,h=24)$mean,col="orange")
lines(forecast(stlStockr,h=24)$mean,col="red",lw=2)
lines(forecast(fitr,h=24)$mean,lw=2,col="purple")
lines(forecast(fitr2,h=24)$mean,lw=2,col="purple")
lines(tsStock,lw=3)
lines(forecast(NETfitr,h=24)$mean,lw=3,lty="longdash",col="brown");
lines(predict(HWStockr,n.ahead=24),lw=2,col="green")
lines(predict(HWStockr_ng,n.ahead=24),lw=2,col="green")
abline(v=2013.25,lty=3)
legend("bottomleft",legend=c("Actual function","prediction - holt winters","prediction - arima(auto)","Prediction = arima(fixed)","prediction - neural nets","prediction - linear model"),col=c("black","green","blue","purple","brown","orange"),lw=2,cex=0.75)

#add all lines if curious
plot(forecast(autofitr,h=24),lw=2,xlim=c(2000,2015.2),ylim=c(-50,100),col="blue",xlab="Time",ylab="Stock Price",main="All 24 predictions competing")
lines(forecast(fitr,h=24)$mean,lw=2,col="purple")
lines(forecast(fitr2,h=24)$mean,lw=2,col="purple")
lines(tsStock,lw=3)
lines(forecast(NETfitr,h=24)$mean,lw=3,lty="longdash",col="brown")
lines(predict(HWStockr,n.ahead=24),lw=2,col="green")
lines(predict(HWStockr_ng,n.ahead=24),lw=2,col="green")
lines(forecast(autofit2,h=24)$mean,lw=2,col="blue")
lines(forecast(fit12,h=24)$mean,lw=2,col="purple")
lines(tsStock,lw=3)
lines(forecast(stlStock1,h=24)$mean,col="yellow",lw=3)
lines(forecast(stlStock2,h=24)$mean,col="yellow",lw=3)
lines(forecast(stlStockr,h=24)$mean,col="yellow",lw=3)
lines(forecast(fit2,h=24)$mean,lw=2,col="purple")
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
legend("bottomleft",legend=c("Actual function","prediction - holt winters","prediction - arima(auto)","prediction - arima (fixed)","prediction - neural nets","prediction - linear model"),col=c("black","green","blue","purple","brown","orange"),lw=2,cex=0.75)
abline(v=2013.25,lty=3)

