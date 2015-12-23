# Tests the code on first 10 stocks
source("findbest.R")

#This function is used to pass the currently selected argument
#to the findbest() function

runner <- function(choice){
    
    BestPrediction = c()
    stockVector = list()
    stockVector[[1]] = read.table(paste("/home/ubuntu/ShinyApps/spredict/stocks/",choice,".csv",sep = ""), sep=",", header=TRUE)
    
    #BestPrediction[1] = findBestPrediction(stockVector[[1]])
    
    check_here = findBestPrediction(stockVector[[1]])
    
    prediction_name = c("Arima auto on actual function","0","0","Neural Networks on actual function",
                        "Holt winters on actual function","0","Arima auto on STL trend",
                        "0","0","0","STL trend on Polynomial","STl on STL of polynomial",
                        "STl on STL of polynomial","Neural Networks on STL trend",
                        "HoltWinters on STL trend","HoltWinters on STL trend without seasonal model",
                        "Arima auto on polynomial trend","TSLM on actual function",
                        "TSLM on polynomial function","TSLM on STL trend","HoltWinters on Polynomial function",
                        "Neural Networks on Polynomial function","Average Prediction of HoltWinters",
                        "Lower bound prediction of HoltWinters","Upper Bound prediction of HoltWinters")
    
    for(i in 1:length(prediction_name)){
      if(check_here[1]==i){
        model_name <- prediction_name[i]
      }
    }
    tobereturned <- c(model_name,check_here[2])
    return (tobereturned)
}
