library(shiny)
library(shinythemes)
source("listfiles.R")

ui <- fluidPage(
  #theme = shinytheme("united"),
  theme = "bootstrap1.css",
  #the following UI must be in a sidebar panel
  titlePanel(
    h1("Stock Analysis", align = "left")),
  
  sidebarLayout(
    sidebarPanel(
      #headerPanel("Stocks"),
      uiOutput("stocknames",align="center"),
      actionButton("calculate","Calculate winning model")
    ),
    mainPanel(
      #actionButton("calculate","Calculate"),
      #actionButton("plothemall","Plot Hemall"),
      #verbatimTextOutput("text"),
      #******************************************************************
      #textOutput("text"),
      #plotOutput("disp"),
      #plotOutput("display"),
      
      htmlOutput("disp"),
      plotOutput("display",height="80px"),
      #htmlOutput("display"),
      textOutput("winner"),
      textOutput("error")
      #plotOutput("alllinestrend")
      #p("POLYNOMIAL TRENDING - ")
      
    )
  ),
  
  #THE FOLLOWING THINGS ARE FOR RESEARCH PURPOSE ONLY

  #actionButton("calculate","calculate"),
  #verbatimTextOutput("text"),
  #verbatimTextOutput("winner"),
  
  #actionButton("plothemall","plothemall"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB1
  #plotOutput("polytrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB2
  #plotOutput("stltrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB3
  #plotOutput("actualtrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB4
  #plotOutput("alllinestrend"),

 #************************************************************** 
  tabsetPanel(position="left",
              tabPanel(title="Polynomial",
                       #value ="output$tab1",
                       plotOutput("polytrend")),
              tabPanel(title="STL",
                       #value="tab2",
                       plotOutput("stltrend")),
              tabPanel(title="Actual",plotOutput("actualtrend")),
              tabPanel(title="All",plotOutput("alllinestrend")),
              #tabPanel(title="All", h1("check check check")), 
              tabPanel(title="Information",
                       h1("TIME SERIES"),
                       h3("1.Overview"),
                       p("
                         A time series is a sequence of data points, typically consisting of successive measurements made over a time interval. Examples of time series are ocean tides, counts of sunspots, and the daily closing value of the Dow Jones Industrial Average. Time series are very frequently plotted via line charts. Time series are used in statistics, signal processing, pattern recognition, econometrics, mathematical finance, weather forecasting, intelligent transport and trajectory forecasting,earthquake prediction, electroencephalography, control engineering, astronomy, communications engineering, and largely in any domain of applied science and engineering which involves temporal measurements.
                         Time series analysis comprises methods for analyzing time series data in order to extract meaningful statistics and other characteristics of the data. Time series forecasting is the use of a model to predict future values based on previously observed values. While regression analysis is often employed in such a way as to test theories that the current values of one or more independent time series affect the current value of another time series, this type of analysis of time series is not called time series analysis, which focuses on comparing values of a single time series or multiple dependent time series at different points in time.
                         Time series data have a natural temporal ordering. This makes time series analysis distinct from cross-sectional studies, in which there is no natural ordering of the observations (e.g. explaining people's wages by reference to their respective education levels, where the individuals' data could be entered in any order). Time series analysis is also distinct from spatial data analysis where the observations typically relate to geographical locations (e.g. accounting for house prices by the location as well as the intrinsic characteristics of the houses). A stochastic model for a time series will generally reflect the fact that observations close together in time will be more closely related than observations further apart. In addition, time series models will often make use of the natural one-way ordering of time so that values for a given period will be expressed as deriving in some way from past values, rather than from future values (see time reversibility.)
                         Time series analysis can be applied to real-valued, continuous data, discrete numeric data, or discrete symbolic data (i.e. sequences of characters, such as letters and words in the English language).
                         An observed time series can be decomposed into three components: the trend (long term direction), the seasonal (systematic, calendar related movements) and the irregular (unsystematic, short term fluctuations).
                         "),
                       p("
                         Time series analysis can be useful to see how a given asset, security or economic variable changes over time or how it changes compared to other variables over the same time period.For example, suppose you wanted to analyze a time series of daily closing stock prices for a given stock over a period of one year. You would obtain a list of all the closing prices for the stock over each day for the past year and list them in chronological order. This would be a one-year, daily closing price time series for the stock.
                         Delving a bit deeper, you might be interested to know if a given stock's time series shows any seasonality, meaning it goes through peaks and valleys at regular times each year. Or you might want to know how a stock's share price changes as an economic variable, such as the unemployment rate, changes.
                         "),
                       h3("2.Methods for Time Series Analysis"),
                       p("
                         Methods for time series analyses may be divided into two classes: frequency-domain methods and time-domain methods. The former include spectral analysis and recently wavelet analysis; the latter include auto-correlation and cross-correlation analysis. In time domain, correlation analyses can be made in a filter-like manner using scaled correlation, thereby mitigating the need to operate in frequency domain.
                         Additionally, time series analysis techniques may be divided into parametric and non-parametric methods. The parametric approaches assume that the underlying stationary stochastic process has a certain structure which can be described using a small number of parameters (for example, using an autoregressive or moving average model). In these approaches, the task is to estimate the parameters of the model that describes the stochastic process. By contrast, non-parametric approaches explicitly estimate the covariance or the spectrum of the process without assuming that the process has any particular structure.
                         Methods of time series analysis may also be divided into linear and non-linear, and univariate and multivariate.
                         "),
                       h3("3.What does a time series show?"),
                       p("
                         A times series allows you to identify change within a population over time. A time series can also show the impact of cyclical, seasonal and irregular events on the data item being measured.
                         Time series can be classified into two different types: stock and flow.
                         A stock series is a measure of certain attributes at a point in time and can be thought of as “stock takes”.
                         A flow series is a series which is a measure of activity over a given period.
                         The main difference between a stock and a flow series is that flow series can contain effects related to the calendar (trading day effects). Both types of series can still be seasonally adjusted using the same seasonal adjustment process.
                         "),
                       h3("4.Seasonal Effects"),
                       p("
                         A seasonal effect is a systematic and calendar related effect. Some examples include the sharp escalation in most Retail series which occurs around December in response to the Christmas period, or an increase in water consumption in summer due to warmer weather. Other seasonal effects include trading day effects (the number of working or trading days in a given month differs from year to year which will impact upon the level of activity in that month) and moving holidays (the timing of holidays such as Easter varies, so the effects of the holiday will be experienced in different periods each year).
                         "),
                       p("
                         Seasonal adjustment is the process of estimating and then removing from a time series influences that are systematic and calendar related. Observed data needs to be seasonally adjusted as seasonal effects can conceal both the true underlying movement in the series, as well as certain non-seasonal characteristics which may be of interest to analysts.
                         "),
                       p("
                         A comparison of original data from the same period in each year does not completely remove all seasonal effects. Certain holidays such as Easter and Chinese New Year fall in different periods in each year, hence they will distort observations. Also, year to year values will be biased by any changes in seasonal patterns that occur over time. For example, consider a comparison between two consecutive March months i.e. compare the level of the original series observed in March for 2000 and 2001. This comparison ignores the moving holiday effect of Easter. Easter occurs in April for most years but if Easter falls in March, the level of activity can vary greatly for that month for some series. This distorts the original estimates. A comparison of these two months will not reflect the underlying pattern of the data. The comparison also ignores trading day effects. If the two consecutive months of March have different composition of trading days, it might reflect different levels of activity in original terms even though the underlying level of activity is unchanged. In a similar way, any changes to seasonal patterns might also be ignored. The original estimates also contains the influence of the irregular component. If the magnitude of the irregular component of a series is strong compared with the magnitude of the trend component, the underlying direction of the series can be distorted.
                         However, the major disadvantage of comparing year to year original data, is lack of precision and time delays in the identification of turning points in a series. Turning points occur when the direction of underlying level of the series changes, for example when a consistently decreasing series begins to rise steadily. If we compare year apart data in the original series, we may miss turning points occurring during the year. For example, if March 2001 has a higher original estimate than March 2000, by comparing these year apart values, we might conclude that the level of activity has increased during the year. However, the series might have increased up to September 2000 and then started to decrease steadily.
                         "),
                       p("
                         When a time series is dominated by the trend or irregular components, it is nearly impossible to identify and remove what little seasonality is present. Hence seasonally adjusting a non-seasonal series is impractical and will often introduce an artificial seasonal element.
                         "),
                       p("
                         The seasonal component consists of effects that are reasonably stable with respect to timing, direction and magnitude. It arises from systematic, calendar related influences such as:
                         Natural Conditions - 
                         Weather fluctuations that are representative of the season
                         (Uncharacteristic weather patterns such as snow in summer would be considered irregular influences).
                         Business and Administrative procedures - 
                         Start and end of the school term.
                         Social and Cultural behaviour - 
                         Christmas.
                         "),
                       p("
                         It also includes calendar related systematic effects that are not stable in their annual timing or are caused by variations in the calendar from year to year, such as:
                         Trading Day Effects - 
                         The number of occurrences of each of the day of the week in a given month will differ from year to year
                          - There were 4 weekends in March in 2000, but 5 weekends in March of 2002.
                         Moving Holiday Effects
                         Holidays which occur each year, but whose exact timing shifts
                         - Easter, Chinese New Year.
                         "),
                       p("
                         Seasonality in a time series can be identified by regularly spaced peaks and troughs which have a consistent direction and approximately the same magnitude every year, relative to the trend. 
                         "),
                       p("
                         The irregular component (sometimes also known as the residual) is what remains after the seasonal and trend components of a time series have been estimated and removed. It results from short term fluctuations in the series which are neither systematic nor predictable. In a highly irregular series, these fluctuations can dominate movements, which will mask the trend and seasonality.
                         "),
                       p("
                         The ABS trend is defined as the 'long term' movement in a time series without calendar related and irregular effects, and is a reflection of the underlying level. It is the result of influences such as population growth, price inflation and general economic changes.
                         "),
                       p("
                         An original time series shows the actual movements in the data over time. An original series includes any movements due to cyclical, seasonal and irregular events.
                         A cyclical effect is any regular fluctuation in daily, weekly, monthly or annual data. For example, the number of commuters using public transport has regular peaks and troughs during each day of the week, depending on the time of day.
                         A seasonal effect is any variation in data due to calendar related effects which occur systematically at specific seasonal frequencies every year. For example, in Australia employment increases over the Christmas/New Year period, or fruit and vegetable prices can vary depending on whether or not they are 'in-season'.
                         An irregular effect is any movement that occurred at a specific point in time, but is unrelated to a season or cycle. For example, a natural disaster, the introduction of legislation, or a one-off major cultural or sporting event.
                         A seasonally adjusted series involves estimating and removing the cyclical and seasonal effects from the original data. Seasonally adjusting a time series is useful if you wish to understand the underlying patterns of change or movement in a population, without the impact of the seasonal or cyclical effects.
                         For example, employment and unemployment are often seasonally adjusted so that the actual change in employment and unemployment levels can be seen, without the impact of periods of peak employment such as Christmas/New Year when a large number of casual workers are temporarily employed. 
                         A trend series is a seasonally adjusted series that has been further adjusted to remove irregular effects and 'smooth' out the series to show the overall 'trend' of the data over time.
                         For example, the trend is often used when analysing economic indicators such as employment and unemployment levels.
                         "),
                       br(),
                       br(),
                       br(),
                       h1("ARIMA"),
                       h3("1.Introduction"),
                       p("In statistics and econometrics, and in particular in time series analysis, an autoregressive integrated moving average (ARIMA) model is a generalization of an autoregressive moving average (ARMA) model. These models are fitted to time series data either to better understand the data or to predict future points in the series (forecasting). They are applied in some cases where data show evidence of non-stationarity, where an initial differencing step (corresponding to the integrated part of the model) can be applied to reduce the non-stationarity.
                          Non-seasonal ARIMA models are generally denoted ARIMA(p, d, q) where parameters p, d, and q are non-negative integers, p is the order of the Autoregressive model, d is the degree of differencing, and q is the order of the Moving-average model. Seasonal ARIMA models are usually denoted ARIMA(p, d, q)(P, D, Q)_m, where m refers to the number of periods in each season, and the uppercase P, D, Q refer to the autoregressive, differencing, and moving average terms for the seasonal part of the ARIMA model.ARIMA models form an important part of the Box-Jenkins approach to time-series modelling.
                          ARIMA models provide another approach to time series forecasting. Exponential smoothing and ARIMA models are the two most widely-used approaches to time series forecasting, and provide complementary approaches to the problem. While exponential smoothing models were based on a description of trend and seasonality in the data, ARIMA models aim to describe the autocorrelations in the data.
                         "),
                       p("
                         ARIMA stands for Autoregressive Integrated Moving Average models. Univariate (single vector) ARIMA is a forecasting technique that projects the future values of a series based entirely on its own inertia. Its main application is in the area of short term forecasting requiring at least 40 historical data points. It works best when your data exhibits a stable or consistent pattern over time with a minimum amount of outliers. Sometimes called Box-Jenkins (after the original authors), ARIMA is usually superior to exponential smoothing techniques when the data is reasonably long and the correlation between past observations is stable. If the data is short or highly volatile, then some smoothing method may perform better. If you do not have at least 38 data points, you should consider some other method than ARIMA. 
                         "),
                       h3("2.ARIMA Modelling of Time Series"),
                       p("Description-
                          Fit an ARIMA model to a univariate time series."),
                       p("Usage - "),
                       pre("
                          arima(x, order = c(0L, 0L, 0L),
                          seasonal = list(order = c(0L, 0L, 0L), period = NA),
                          xreg = NULL, include.mean = TRUE,
                          transform.pars = TRUE,
                          fixed = NULL, init = NULL,
                          method = c('CSS-ML', 'ML', 'CSS'), n.cond,
                          SSinit = c('Gardner1980', 'Rossignol2011'),
                          optim.method = 'BFGS',
                          optim.control = list(), kappa = 1e6)
                           "),
                       p("Arguments - "),
                       pre("
x - univariate time series
order - A specification of the non-seasonal part of the ARIMA model: the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.
seasonal - A specification of the seasonal part of the ARIMA model, plus the period (which defaults to frequency(x)). 
           This should be a list with components order and period, but a specification of just a numeric vector of length 3 will be turned into a suitable list with the specification as the order.
xreg - Optionally, a vector or matrix of external regressors, which must have the same number of rows as x.
include.mean - Should the ARMA model include a mean/intercept term? The default is TRUE for undifferenced series, and it is ignored for ARIMA models with differencing.
transforms.pars - logical; if true, the AR parameters are transformed to ensure that they remain in the region of stationarity.
                  Not used for method = 'CSS'. For method = 'ML', it has been advantageous to set transform.pars = FALSE in some cases, see also fixed.
fixed - optional numeric vector of the same length as the total number of parameters.
        If supplied, only NA entries in fixed will be varied. transform.pars = TRUE will be overridden (with a warning) if any AR parameters are fixed.
        It may be wise to set transform.pars = FALSE when fixing MA parameters, especially near non-invertibility. 
        init - optional numeric vector of initial parameter values. Missing values will be filled in, by zeroes except for regression coefficients. 
        Values already specified in fixed will be ignored.
method - fitting method: maximum likelihood or minimize conditional sum-of-squares.
         The default (unless there are missing values) is to use conditional-sum-of-squares to find starting values, then maximum likelihood. Can be abbreviated.
n.cond - only used if fitting by conditional-sum-of-squares: the number of initial observations to ignore. It will be ignored if less than the maximum lag of an AR term.
SSinit - a string specifying the algorithm to compute the state-space initialization of the likelihood.
optim.method - The value passed as the method argument to optim.
optim.control - List of control parameters for optim.
kappa - the prior variance (as a multiple of the innovations variance) for the past observations in a differenced model. Do not reduce this.
                           "),
                       p("Examples - "),
                       pre("
arima(lh, order = c(1,0,0))
arima(lh, order = c(3,0,0))
arima(lh, order = c(1,0,1))

arima(lh, order = c(3,0,0), method = 'CSS')

arima(USAccDeaths, order = c(0,1,1), seasonal = list(order = c(0,1,1)))
arima(USAccDeaths, order = c(0,1,1), seasonal = list(order = c(0,1,1)),
      method = 'CSS') # drops first 13 observations.
# for a model with as few years as this, we want full ML

arima(LakeHuron, order = c(2,0,0), xreg = time(LakeHuron) - 1920)

## presidents contains NAs
## graphs in example(acf) suggest order 1 or 3
require(graphics)
(fit1 <- arima(presidents, c(1, 0, 0)))
nobs(fit1)
tsdiag(fit1)
(fit3 <- arima(presidents, c(3, 0, 0)))  # smaller AIC
tsdiag(fit3)
BIC(fit1, fit3)
## compare a whole set of models; BIC() would choose the smallest
AIC(fit1, arima(presidents, c(2,0,0)),
          arima(presidents, c(2,0,1)), # <- chosen (barely) by AIC
    fit3, arima(presidents, c(3,0,1)))

## An example of ARIMA forecasting:
predict(fit3, 3)
                           "),
                       h3("3.Basic Concepts"),
                       p("
                         The first step in applying ARIMA methodology is to check for stationarity.Stationarity implies that the series remains at a fairly constant level over time. If a trend exists, as in most economic or business applications, then your data is NOT stationary. The data should also show a constant variance in its fluctuations over time. This is easily seen with a series that is heavily seasonal and growing at a faster rate. In such a case, the ups and downs in the seasonality will become more dramatic over time. Without these stationarity conditions being met, many of the calculations associated with the process cannot be computed. 
                         "),
                       h3("4.Differencing"),
                       p("
                         If a graphical plot of the data indicates nonstationarity, then you should difference the series. Differencing is an excellent way of transforming a nonstationary series to a stationary one. This is done by subtracting the observation in the current period from the previous one. If this transformation is done only once to a series, you say that the data has been first differenced. This process essentially eliminates the trend if your series is growing at a fairly constant rate. If it is growing at an increasing rate, you can apply the same procedure and difference the data again. Your data would then be second differenced. 
                         "),
                       h3("5.Autocorrelations"),
                       p("
                         Autocorrelations are numerical values that indicate how a data series is related to itself over time. More precisely, it measures how strongly data values at a specified number of periods apart are correlated to each other over time. The number of periods apart is usually called the lag. For example, an autocorrelation at lag 1 measures how values 1 period apart are correlated to one another throughout the series. An autocorrelation at lag 2 measures how the data two periods apart are correlated throughout the series. Autocorrelations may range from +1 to -1. A value close to +1 indicates a high positive correlation while a value close to -1 implies a high negative correlation. These measures are most often evaluated through graphical plots called correlagrams. A correlagram plots the auto- correlation values for a given series at different lags. This is referred to as the autocorrelation function and is very important in the ARIMA method. 
                         "),
                       h3("6.Auto-Regressive Models"),
                       p("
                          ARIMA methodology attempts to describe the movements in a stationary time series as a function of what are called autoregressive and moving average parameters. These are referred to as AR parameters (autoregessive) and MA parameters (moving averages). An AR model with only 1 parameter may be written as - 
                          X(t) = A(1) * X(t-1) + E(t)
                          where X(t) = time series under investigation
                          A(1) = the autoregressive parameter of order 1
                          X(t-1) = the time series lagged 1 period
                          E(t) = the error term of the model
                          This simply means that any given value X(t) can be explained by some function of its previous value, X(t-1), plus some unexplainable random error, E(t). If the estimated value of A(1) was .30, then the current value of the series would be related to 30% of its value 1 period ago. Of course, the series could be related to more than just one past value. For example,
                          X(t) = A(1) * X(t-1) + A(2) * X(t-2) + E(t)
                          This indicates that the current value of the series is a combination of the two immediately preceding values, X(t-1) and X(t-2), plus some random error E(t). Our model is now an autoregressive model of order 2
                         "),
                       h3("Moving Average Models"),
                       p(" A second type of Box-Jenkins model is called a moving average model. Although these models look very similar to the AR model, the concept behind them is quite different. Moving average parameters relate what happens in period t only to the random errors that occurred in past time periods, i.e. E(t-1), E(t-2), etc. rather than to X(t-1), X(t-2), (Xt-3) as in the autoregressive approaches. A moving average model with one MA term may be written as follows - 
                           X(t) = -B(1) * E(t-1) + E(t)
                           The term B(1) is called an MA of order 1. The negative sign in front of the parameter is used for convention only and is usually printed out auto- matically by most computer programs. The above model simply says that any given value of X(t) is directly related only to the random error in the previous period, E(t-1), and to the current error term, E(t). As in the case of autoregressive models, the moving average models can be extended to higher order structures covering different combinations and moving average lengths.
                         "),
                       h3("7.Mixed Models"),
                       p("
                          ARIMA methodology also allows models to be built that incorporate both autoregressive and moving average parameters together. These models are often referred to as mixed models. Although this makes for a more complicated forecasting tool, the structure may indeed simulate the series better and produce a more accurate forecast. Pure models imply that the structure consists only of AR or MA parameters - not both.
                          The models developed by this approach are usually called ARIMA models because they use a combination of autoregressive (AR), integration (I) - referring to the reverse process of differencing to produce the forecast, and moving average (MA) operations. An ARIMA model is usually stated as ARIMA(p,d,q). This represents the order of the autoregressive components (p), the number of differencing operators (d), and the highest order of the moving average term. For example, ARIMA(2,1,1) means that you have a second order autoregressive model with a first order moving average component whose series has been differenced once to induce stationarity. 
                         "),
                       br(),
                       br(),
                       br(),
                       h1("HOLT-WINTERS FILTERING"),
                       h3("1.Overview"),
                       p("
                         Exponential smoothing is a rule of thumb technique for smoothing time series data, particularly for recursively applying as many as 3 Low-pass filters with exponential window functions. Such techniques have broad application that is not intended to be strictly accurate or reliable for every situation. It is an easily learned and easily applied procedure for approximately calculating or recalling some value, or for making some determination based on prior assumptions by the user, such as seasonality. Like any application of repeated low-pass filtering, the observed phenomenon may be an essentially random process, or it may be an orderly, but noisy, process. Whereas in the simple moving average the past observations are weighted equally, exponential window functions assign exponentially decreasing weights over time. The use of three filters is based on empirical evidence and broad application, as well as its role in prime number theory as the second most common prime number and extensive use in numerology.
                         Exponential smoothing is commonly applied to smooth data, as many window functions are in signal processing, acting as low-pass filters to remove high frequency noise. This method parrots Poisson's use of recursive exponential window functions in convolutions from the 19th century, as well as Kolmogorov and Zurbenko's use of recursive moving averages from their studies of turbulence in the 1940s. See Kolmogorov-Zurbenko filter for more information.
                         "),
                       p("
                         Holt (1957) and Winters (1960) extended Holt’s method to capture seasonality. The Holt-Winters seasonal method comprises the forecast equation and three smoothing equations — one for the level ℓt, one for trend bt, and one for the seasonal component denoted by st, with smoothing parameters α, β∗ and γ. We use m to denote the period of the seasonality, i.e., the number of seasons in a year. For example, for quarterly data m=4, and for monthly data m=12.
                         There are two variations to this method that differ in the nature of the seasonal component. The additive method is preferred when the seasonal variations are roughly constant through the series, while the multiplicative method is preferred when the seasonal variations are changing proportional to the level of the series. With the additive method, the seasonal component is expressed in absolute terms in the scale of the observed series, and in the level equation the series is seasonally adjusted by subtracting the seasonal component. Within each year the seasonal component will add up to approximately zero. With the multiplicative method, the seasonal component is expressed in relative terms (percentages) and the series is seasonally adjusted by dividing through by the seasonal component. Within each year, the seasonal component will sum up to approximately m.
                         "),
                       p("
                           Many companies use the Holt-Winters (HW) method to produce short-term demand forecasts when their sales data contain a trend and 
                           a  seasonal  pattern.  Fifty  years  old  this  year, the  method  is  popular  because  it  is  simple, 
                           has  low  data-storage  requirements,  and  is easily automated. It also has the advantage of being able to adapt to changes in trends and 
                           seasonal  patterns  in  sales  when  they  occur. This means that slowdowns or speed-ups in demand,  or  changing  consumer  behavior 
                           at  Christmas  or  in  the  summer,  can  all  be accommodated.  It  achieves  this  by  updating its estimates of these patterns as soon as 
                           each new sales figure arrives.
                         "),
                       p("
                         Over  the  years,  the  Holt-Winters  method has  been  adapted  for  use  in  several  important situations not originally examined by its creators.More  specifically,  researchers  have 
                         recently looked at three issues:
                         How can we stop the method from being unduly influenced by sales figures that are unusually high or low (i.e., outliers)?
                         Is the method useful when there are several different  seasonal  patterns  in  sales  (such as  when  demand  has  hourly,  daily,  and monthly cycles mixed together)? 
                         How  can  we  obtain  reliable  prediction intervals  from  the  method?  For  example, we  might  want  the  method  to  give  us  a range  for  next  month’s  sales  so  that  there is  a  90%  chance  that  sales  will  fall  within this range. But how can we ensure that we really  do  have  a  90%  chance  of  capturing sales within the range?
                         "),
                       p("
                         While  Holt-Winters  remains  a  mainstay approach to business forecasting, it has recently  been  extended  to  deal  with  three problem areas.
                         One  is  the  presence  of  unusual  values (outliers) left  unattended, outliers  can distort hW forecasts
                         Another is the prevalence of multiple seasonal cycles,such as acombination of day-of-week patterns  and  month-of-year  patterns. Traditional hW could account for only a single seasonal pattern.
                         Third  is  the  need  for  prediction  intervals, which affect safety-stock calculations, among other things. Traditional hW intervals in use tend  to  be  too  narrow,  misleading  us  into thinking our forecasts are more precise than they really turn out to be.
                         "),
                       p("
                         The  Holt-Winters  method  was  designed  to handle  data  where  there  is  a  conventional seasonal  cycle  across  the  course  of  a  year, such as monthly seasonality. However, many series  have multiple  cycles:  the  demand  for electricity  will  have  hourly  (patterns  across the  hours  of  a  day),  daily  (patterns  across the  days  of  the  week),  and  monthly  cycles. 
                         Similar patterns occur in the number of calls received by call centers or the workload faced by hospitals. 
                         "),
                       h3("2.Prediction Function for Fitted Holt-Winters Models"),
                       p("
                         Description - Computes predictions and prediction intervals for models fitted by the Holt-Winters method. 
                         "),
                       p("Usage - "),
                       pre("
                           ## S3 method for class 'HoltWinters'
                           predict(object, n.ahead = 1, prediction.interval = FALSE,level = 0.95, ...)
                           "),
                       p("Arguments - "),
                       pre("
object - An object of class HoltWinters.
n.ahead - Number of future periods to predict.
prediction.interval  - logical. If TRUE, the lower and upper bounds of the corresponding prediction intervals are computed.
level - Confidence level for the prediction interval.
                           "),
                       p("
                         Value - A time series of the predicted values. If prediction intervals are requested, a multiple time series is returned with columns fit, lwr and upr for the predicted values and the lower and upper bounds respectively. 
                         "),
                       p("Examples - "),
                       pre("
require(graphics)
## Seasonal Holt-Winters
(m <- HoltWinters(co2))
plot(m)
plot(fitted(m))

(m <- HoltWinters(AirPassengers, seasonal = 'mult'))
plot(m)

## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = FALSE)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)
                           "),
                       br(),
                       br(),
                       br(),
                       h1("NEURAL NETWORKS"),
                       h3("1.Overview"),
                       p("
                         Neural Networks are a different paradigm for computing:
                         von Neumann machines are based on the processing/memory abstraction of human information processing.
                         Neural networks are based on the parallel architecture of animal brains.
                         Neural networks are a form of multiprocessor computer system, with
                         simple processing elements,a high degree of interconnection,simple scalar messages,adaptive interaction between elements.
                         "),
                       p("
                         Neural Networks have seen an explosion of interest over the last few years, and are being successfully applied across an extraordinary range of problem domains, in areas as diverse as finance, medicine, engineering, geology and physics. Indeed, anywhere that there are problems of prediction, classification or control, neural networks are being introduced. This sweeping success can be attributed to a few key factors:
                         Power:Neural networks are very sophisticated modeling techniques capable of modeling extremely complex functions. In particular, neural networks are nonlinear (a term which is discussed in more detail later in this section). For many years linear modeling has been the commonly used technique in most modeling domains since linear models have well-known optimization strategies. Where the linear approximation was not valid (which was frequently the case) the models suffered accordingly. Neural networks also keep in check the curse of dimensionality problem that bedevils attempts to model nonlinear functions with large numbers of variables.
                         Ease of use:Neural networks learn by example. The neural network user gathers representative data, and then invokes training algorithms to automatically learn the structure of the data. Although the user does need to have some heuristic knowledge of how to select and prepare data, how to select an appropriate neural network, and how to interpret the results, the level of user knowledge needed to successfully apply neural networks is much lower than would be the case using (for example) some more traditional nonlinear statistical methods. 
                         Neural networks are also intuitively appealing, based as they are on a crude low-level model of biological neural systems. In the future, the development of this neurobiological modeling may lead to genuinely intelligent computers.
                         "),
                       h3("2.Applications"),
                       p("
                         Neural networks are applicable in virtually every situation in which a relationship between the predictor variables (independents, inputs) and predicted variables (dependents, outputs) exists, even when that relationship is very complex and not easy to articulate in the usual terms of correlations or differences between groups.
                         "),
                       p("
                         Detection of medical phenomena. A variety of health-related indices (e.g., a combination of heart rate, levels of various substances in the blood, respiration rate) can be monitored. The onset of a particular medical condition could be associated with a very complex (e.g., nonlinear and interactive) combination of changes on a subset of the variables being monitored. Neural networks have been used to recognize this predictive pattern so that the appropriate treatment can be prescribed. 
                         "),
                       p("
                         Stock market prediction. Fluctuations of stock prices and stock indices are another example of a complex, multidimensional, but in some circumstances at least partially-deterministic phenomenon. Neural networks are being used by many technical analysts to make predictions about stock prices based upon a large number of factors such as past performance of other stocks and various economic indicators. 
                         "),
                       p("
                         Credit assignment. A variety of pieces of information are usually known about an applicant for a loan. For instance, the applicant's age, education, occupation, and many other facts may be available. After training a neural network on historical data, neural network analysis can identify the most relevant characteristics and use those to classify applicants as good or bad credit risks.
                         "),
                       p("
                         Monitoring the condition of machinery. Neural networks can be instrumental in cutting costs by bringing additional expertise to scheduling the preventive maintenance of machines. A neural network can be trained to distinguish between the sounds a machine makes when it is running normally (false alarms) versus when it is on the verge of a problem. After this training period, the expertise of the network can be used to warn a technician of an upcoming breakdown, before it occurs and causes costly unforeseen downtime.
                         "),
                       p("
                         Engine management. Neural networks have been used to analyze the input of sensors from an engine. The neural network controls the various parameters within which the engine functions, in order to achieve a particular goal, such as minimizing fuel consumption. 
                         "),
                       p("
                         Some other applications are Currency prediction , Futures prediction , Bond ratings , Business failure prediction , Debt risk assessment , Credit approval , Bank theft , Bank failure.
                         "),
                       h3("3.The Biological Inspiration"),
                       p("
                         Neural networks grew out of research in Artificial Intelligence; specifically, attempts to mimic the fault-tolerance and capacity to learn of biological neural systems by modeling the low-level structure of the brain (see Patterson, 1996). The main branch of Artificial Intelligence research in the 1960s -1980s produced Expert Systems. These are based upon a high-level model of reasoning processes (specifically, the concept that our reasoning processes are built upon manipulation of symbols). It became rapidly apparent that these systems, although very useful in some domains, failed to capture certain key aspects of human intelligence. According to one line of speculation, this was due to their failure to mimic the underlying structure of the brain. In order to reproduce intelligence, it would be necessary to build systems with a similar architecture.
                         "),
                       p("
                         The brain is principally composed of a very large number (circa 10,000,000,000) of neurons, massively interconnected (with an average of several thousand interconnects per neuron, although this varies enormously). Each neuron is a specialized cell which can propagate an electrochemical signal. The neuron has a branching input structure (the dendrites), a cell body, and a branching output structure (the axon). The axons of one cell connect to the dendrites of another via a synapse. When a neuron is activated, it fires an electrochemical signal along the axon. This signal crosses the synapses to other neurons, which may in turn fire. A neuron fires only if the total signal received at the cell body from the dendrites exceeds a certain level (the firing threshold).
                         "),
                       p("
                         The strength of the signal received by a neuron (and therefore its chances of firing) critically depends on the efficacy of the synapses. Each synapse actually contains a gap, with neurotransmitter chemicals poised to transmit a signal across the gap. One of the most influential researchers into neurological systems (Donald Hebb) postulated that learning consisted principally in altering the strength of synaptic connections. For example, in the classic Pavlovian conditioning experiment, where a bell is rung just before dinner is delivered to a dog, the dog rapidly learns to associate the ringing of a bell with the eating of food. The synaptic connections between the appropriate part of the auditory cortex and the salivation glands are strengthened, so that when the auditory cortex is stimulated by the sound of the bell the dog starts to salivate. Recent research in cognitive science, in particular in the area of nonconscious information processing, have further demonstrated the enormous capacity of the human mind to infer (learn) simple input-output covariations from extremely complex stimuli (e.g., see Lewicki, Hill, and Czyzewska, 1992).
                         "),
                       p("
                         Thus, from a very large number of extremely simple processing units (each performing a weighted sum of its inputs, and then firing a binary signal if the total input exceeds a certain level) the brain manages to perform extremely complex tasks. Of course, there is a great deal of complexity in the brain which has not been discussed here, but it is interesting that artificial neural networks can achieve some remarkable results using a model not much more complex than this.
                         "),
                       h3("4.The Basic Artificial Model"),
                       p("
                         To capture the essence of biological neural systems, an artificial neuron is defined as follows:
                         It receives a number of inputs (either from original data, or from the output of other neurons in the neural network). Each input comes via a connection that has a strength (or weight); these weights correspond to synaptic efficacy in a biological neuron. Each neuron also has a single threshold value. The weighted sum of the inputs is formed, and the threshold subtracted, to compose the activation of the neuron (also known as the post-synaptic potential, or PSP, of the neuron).
                         The activation signal is passed through an activation function (also known as a transfer function) to produce the output of the neuron. 
                         "),
                       p("
                         If the step activation function is used (i.e., the neuron's output is 0 if the input is less than zero, and 1 if the input is greater than or equal to 0) then the neuron acts just like the biological neuron described earlier (subtracting the threshold from the weighted sum and comparing with zero is equivalent to comparing the weighted sum to the threshold). Actually, the step function is rarely used in artificial neural networks, as will be discussed. Note also that weights can be negative, which implies that the synapse has an inhibitory rather than excitatory effect on the neuron: inhibitory neurons are found in the brain.
                         "),
                       p("
                         This describes an individual neuron. The next question is: how should neurons be connected together? If a network is to be of any use, there must be inputs (which carry the values of variables of interest in the outside world) and outputs (which form predictions, or control signals). Inputs and outputs correspond to sensory and motor nerves such as those coming from the eyes and leading to the hands. However, there also can be hidden neurons that play an internal role in the network. The input, hidden and output neurons need to be connected together.
                         "),
                       p("
                         The key issue here is feedback (Haykin, 1994). A simple network has a feedforward structure: signals flow from inputs, forwards through any hidden units, eventually reaching the output units. Such a structure has stable behavior. However, if the network is recurrent (contains connections back from later to earlier neurons) it can be unstable, and has very complex dynamics. Recurrent networks are very interesting to researchers in neural networks, but so far it is the feedforward structures that have proved most useful in solving real problems.
                         "),
                       p("
                         A typical feedforward network has neurons arranged in a distinct layered topology. The input layer is not really neural at all: these units simply serve to introduce the values of the input variables. The hidden and output layer neurons are each connected to all of the units in the preceding layer. Again, it is possible to define networks that are partially-connected to only some units in the preceding layer; however, for most applications fully-connected networks are better.
                         "),
                       p("
                         When the network is executed (used), the input variable values are placed in the input units, and then the hidden and output layer units are progressively executed. Each of them calculates its activation value by taking the weighted sum of the outputs of the units in the preceding layer, and subtracting the threshold. The activation value is passed through the activation function to produce the output of the neuron. When the entire network has been executed, the outputs of the output layer act as the output of the entire network.
                         "),
                       h3("5.Using A Neural Network"),
                       p("
                         The type of problem amenable to solution by a neural network is defined by the way they work and the way they are trained. Neural networks work by feeding in some input variables, and producing some output variables. They can therefore be used where you have some known information, and would like to infer some unknown information (see Patterson, 1996; Fausett, 1994). Some examples are:
                         Stock market prediction. You know last week's stock prices and today's DOW, NASDAQ, or FTSE index; you want to know tomorrow's stock prices.
                         Credit assignment. You want to know whether an applicant for a loan is a good or bad credit risk. You usually know applicants' income, previous credit history, etc. (because you ask them these things).
                         Control. You want to know whether a robot should turn left, turn right, or move forwards in order to reach a target; you know the scene that the robot's camera is currently observing.
                         "),
                       p("
                         Needless to say, not every problem can be solved by a neural network. You may wish to know next week's lottery result, and know your shoe size, but there is no relationship between the two. Indeed, if the lottery is being run correctly, there is no fact you could possibly know that would allow you to infer next week's result. Many financial institutions use, or have experimented with, neural networks for stock market prediction, so it is likely that any trends predictable by neural techniques are already discounted by the market, and (unfortunately), unless you have a sophisticated understanding of that problem domain, you are unlikely to have any success there either.
                         "),
                       p("
                         Therefore, another important requirement for the use of a neural network therefore is that you know (or at least strongly suspect) that there is a relationship between the proposed known inputs and unknown outputs. This relationship may be noisy (you certainly would not expect that the factors given in the stock market prediction example above could give an exact prediction, as prices are clearly influenced by other factors not represented in the input set, and there may be an element of pure randomness) but it must exist.
                         "),
                       p("
                         In general, if you use a neural network, you won't know the exact nature of the relationship between inputs and outputs - if you knew the relationship, you would model it directly. The other key feature of neural networks is that they learn the input/output relationship through training. There are two types of training used in neural networks, with different types of networks using different types of training. These are supervised and unsupervised training, of which supervised is the most common and will be discussed in this section (unsupervised learning is described in a later section).
                         "),
                       p("
                         In supervised learning, the network user assembles a set of training data. The training data contains examples of inputs together with the corresponding outputs, and the network learns to infer the relationship between the two. Training data is usually taken from historical records. In the above examples, this might include previous stock prices and DOW, NASDAQ, or FTSE indices, records of previous successful loan applicants, including questionnaires and a record of whether they defaulted or not, or sample robot positions and the correct reaction.
                         "),
                       p("
                         The neural network is then trained using one of the supervised learning algorithms (of which the best known example is back propagation, devised by Rumelhart et. al., 1986), which uses the data to adjust the network's weights and thresholds so as to minimize the error in its predictions on the training set. If the network is properly trained, it has then learned to model the (unknown) function that relates the input variables to the output variables, and can subsequently be used to make predictions where the output is not known.
                         "),
                       h3("6.Gathering Data for Neural Networks"),
                       p("
                         Once you have decided on a problem to solve using neural networks, you will need to gather data for training purposes. The training data set includes a number of cases, each containing values for a range of input and output variables. The first decisions you will need to make are: which variables to use, and how many (and which) cases to gather.
                         "),
                       p("
                         The choice of variables (at least initially) is guided by intuition. Your own expertise in the problem domain will give you some idea of which input variables are likely to be influential. As a first pass, you should include any variables that you think could have an influence - part of the design process will be to whittle this set down.
                         "),
                       p("
                         Neural networks process numeric data in a fairly limited range. This presents a problem if data is in an unusual range, if there is missing data, or if data is non-numeric. Fortunately, there are methods to deal with each of these problems. Numeric data is scaled into an appropriate range for the network, and missing values can be substituted for using the mean value (or other statistic) of that variable across the other available training cases (see Bishop, 1995).
                         "),
                       p("
                         Handling non-numeric data is more difficult. The most common form of non-numeric data consists of nominal-value variables such as Gender={Male, Female}. Nominal-valued variables can be represented numerically. However, neural networks do not tend to perform well with nominal variables that have a large number of possible values.
                         "),
                       p("
                         For example, consider a neural network being trained to estimate the value of houses. The price of houses depends critically on the area of a city in which they are located. A particular city might be subdivided into dozens of named locations, and so it might seem natural to use a nominal-valued variable representing these locations. Unfortunately, it would be very difficult to train a neural network under these circumstances, and a more credible approach would be to assign ratings (based on expert knowledge) to each area; for example, you might assign ratings for the quality of local schools, convenient access to leisure facilities, etc.
                         "),
                       p("
                         Other kinds of non-numeric data must either be converted to numeric form, or discarded. Dates and times, if important, can be converted to an offset value from a starting date/time. Currency values can easily be converted. Unconstrained text fields (such as names) cannot be handled and should be discarded.
                         "),
                       p("
                         The number of cases required for neural network training frequently presents difficulties. There are some heuristic guidelines, which relate the number of cases needed to the size of the network (the simplest of these says that there should be ten times as many cases as connections in the network). Actually, the number needed is also related to the (unknown) complexity of the underlying function which the network is trying to model, and to the variance of the additive noise. As the number of variables increases, the number of cases required increases nonlinearly, so that with even a fairly small number of variables (perhaps fifty or less) a huge number of cases are required. This problem is known as the curse of dimensionality,and is discussed further later in this section.
                         "),
                       p("
                         For most practical problem domains, the number of cases required will be hundreds or thousands. For very complex problems more may be required, but it would be a rare (even trivial) problem which required less than a hundred cases. If your data is sparser than this, you really don't have enough information to train a network, and the best you can do is probably to fit a linear model. If you have a larger, but still restricted, data set, you can compensate to some extent by forming an ensemble of networks, each trained using a different resampling of the available data, and then average across the predictions of the networks in the ensemble.
                         "),
                       p("
                         Many practical problems suffer from data that is unreliable: some variables may be corrupted by noise, or values may be missing altogether. Neural networks are also noise tolerant. However, there is a limit to this tolerance; if there are occasional outliers far outside the range of normal values for a variable, they may bias the training. The best approach to such outliers is to identify and remove them (either discarding the case, or converting the outlier into a missing value). 
                         "),
                       h3("7.Artificial Neural Network & Stock Market Prediction"),
                       p("
                         With the advent of the digital computer, stock market prediction has since moved into the technological realm. The most prominent technique involves the use of artificial neural networks (ANNs) and Genetic Algorithms. Scholars found bacterial chemotaxis optimization method may perform better than GA.ANNs can be thought of as mathematical function approximators. The use of ANN simulates how human brain functions, by feeding computers with massive data to mimic human thinking. The most common form of ANN in use for stock market prediction is the feed forward network utilising the backward propagation of errors algorithm to update the network weights. These networks are commonly referred to as Backpropagation networks. Another form of ANN that is more appropriate for stock prediction is the time recurrent neural network (RNN) or time delay neural network (TDNN). Examples of RNNN and TDNN are the Elman, Jordan, and Elman-Jordan networks. (See the Elman And Jordan Networks).
                         For stock prediction with ANNs, there are usually two approaches taken for forecasting different time horizons: independent and joint. The independent approach employs a single ANN for each time horizon, for example, 1-day, 2-day, or 5-day. The advantage of this approach is that network forecasting error for one horizon won't impact the error for another horizon—since each time horizon is typically a unique problem. The joint approach, however, incorporates multiple time horizons together so that they are determined simultaneously. In this approach, forecasting error for one time horizon may share its error with that of another horizon, which can decrease performance. There are also more parameters required for a joint model, which increases the risk of overfitting.
                         Of late, the majority of academic research groups studying ANNs for stock forecasting seem to be using an ensemble of independent ANNs methods more frequently, with greater success. An ensemble of ANNs would use low price and time lags to predict future lows, while another network would use lagged highs to predict future highs. The predicted low and high predictions are then used to form stop prices for buying or selling. Outputs from the individual low and high networks can also be input into a final network that would also incorporate volume, intermarket data or statistical summaries of prices, leading to a final ensemble output that would trigger buying, selling, or market directional change. A major finding with ANNs and stock prediction is that a classification approach (vs. function approximation) using outputs in the form of buy(y=+1) and sell(y=-1) results in better predictive reliability than a quantitative output such as low or high price.This is explained by the fact that an ANN can predict class better than a quantitative value as in function approximation—since ANNs occasionally learn more about the noise in the input data.
                         Since NNs require training and can have a large parameter space, it is useful to modify the network structure for optimal predictive ability.
                         "),
                       p("
                         Fundamental analysis is the physical study of a company in terms of its product sales, manpower, quality, infrastructure etc.  to  understand  it  standing  in  the  market  and  thereby  its profitability as an investment .
                         The fundamental analysts believe that the market is defined 90 percent by logical and 10 percent by physiological factors.
                         But, this analysis is not suitable for our study because the data it uses to determine the  intrinsic  value  of  an  asset  does  not  change  on  daily basis  and  therefore  is  not  suitable  for  shortterm  basis.
                         However,  this  analysis  is  suitable  for  predicting  the  share market only in long term basis.
                         "),
                       p("
                         The  technical  analysis  predicts  the  appropriate  time  to  buy or sell a share. Technical analysts use charts which contain technical data like price, volume, highest and lowest prices per trading to predict future share  movements.
                         Price charts are  used  to  recognize  trends.  These  trends  are  understood by  supply  and  demand  issues  that  often  have  cyclical  or some sort of noticeable patterns.
                         To understand a company and  its  profitability  through  its  share  prices  in  the  market, some  parameters  can  guide  an  investor  towards  making  a careful  decision.  These  parameters  are  termed  Indicators and Oscillators.
                         This is a very popular approach used to predict  the  market.
                         But  the  problem  of  this  analysis  is  that the  extraction  of  trading  rules  from  the  study  of  charts  is highly  subjective,  as  a  result  different  analysts  extract different   trading   rulesstudying   the   same   charts.
                         This analysis  can  be  used  to  predict  the  market  price  on  daily basis  but  we  will  not  use  this  approach  because  of  its subjective nature.
                         "),
                       p("
                         Machine   learning   approach   is   attractive   for   artificial intelligence  since  it  is  based  on  the  principle  of  learning from  training  and  experience.  
                         Connectionist  models such  as  ANNs  are  well  suited  for  machine  learning  where connection weights adjusted to improve the performance of a network.
                         "),
                       p("
                         The  main  problem in  predicting  share  market  is  that  the share  market  is  a  chaos  system.  
                         There  are  many  variables that  could  affect  the  share  market  directly  or  indirectly. 
                         There are no significant relations between the variables and the   price.   
                         We   cannot   draw   any   mathematical   relation among  the  variables.  
                         There  are  no  laws  of  predicting  the share price using these variables.
                         "),
                       pre("
                           USE OF BACKFEEDING ALGORITHM FOR USING NEURAL NETWORK IN STOCK MARKET PREDICTION
                           Step 1:Feed the normalized input data sample, compute the corresponding output                              
                           Step 2:Compute the error between the output(s) and the actual target(s)
                           Step 3:The connection weights and membership functions are adjusted
                           Step 4:IF Error > Tolerance THEN goto Step 1 ELSE stop
                           "),
                       pre("
INPUTS USED IN NEURAL NETWORKS AFFECTING STOCK PRICE
1.General  index  is  a  number  that  measure  the  relative  value of  a  section  of  share  market.  It  reflects  the  total  economic condition  of  the  market. 
  If  the  general  index  goes  down then  it  means  the  economic  condition  of  that  particular market is relatively in poor condition. 
2.The Net asset value (NAV) of a company is the company‟s total  assets  minus  its  total  liabilities.  NAV  is  typically calculated on a per-share basis.
  NAV= (Net asset of a company− Liability)/ Total number of outstanding share
  NAV is  also  calculated  each  day  by  taking  the  last  market value  of  all  securities  owned  plus  all  other  assets  such  as cash,  subtracting  all  liabilities  and  then  dividing  the  result by the total number of shares outstanding.
  NAV reflects the financial condition of the company. We can judge the company reputation by the NAV.
3.The P/E ratio makes a relationship between the share price and the company‟s earnings. 
  The P/E ratio of a share is a measure of the price paid for a share relative to the annual net income or profit earned by the firm per share.
  P/E ratio = Share Price / Earnings Per Share. 
  If  P/E  ratio  rises  then  there  is  a  tendency  of  the  company share  price  falls,  the  higher  P/E  ratio  then  the  higher probability to decrease the price.
4.Earnings per share (EPS) is a comparison tool between two companies. 
  Earnings  per  shareserve  as  an  indicator  of a company's profitability.
  EPS =Net Earnings / Number of Outstanding Shares.
  For output we use the Price of the share. Using this data set we trained the network.
5.Share  volume  can  be  calculated  in  two  different  types  the daily share volume and the monthly share volume. the total number  of  share  is  sold  in  a  particular  day  is  called  daily share  volume.  
  In  monthly  share  volume  is  the  sum  of  the trading volumes during that month.
                           "),
                       p("
                         Neural networks (NNs), as artificial intelligence (AI) methods, have become very important in making  stock market predictions. Much  research on the  applications of NNs  for solving business problems have proven their advantages over statistical and other methods that do not include AI, although there is no optimal methodology for a certain problem. 
                         In order toidentify the main benefits and limitations of previous methods in NN applications and to findconnections between methodology and problem domains, data models, and results obtained,  acomparative analysis of  selected applications is conducted. 
                         It can be concluded from analysisthat  NNs are most implemented in forecasting stock prices, returns, and stock modeling, and themost frequent  methodology  is the Backpropagation algorithm. 
                         However,  the importance of NNintegration with other artificial intelligence methods is emphasized by numerous authors. 
                         Inspiteof many benefits, there are limitations that should be investigated, such as the relevance of theresults, and the best topology for the certain problems.
                         ")
              ),
              id="tabs", selected = NULL
  )

)
