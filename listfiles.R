#this function is used to list down all the files required for the stock analysis. 
#Required in the selectInput(choices param)
listfiles <- function(){
  
    files <- Sys.glob("/home/ubuntu/ShinyApps/spredict/stocks/*.csv")
    
    l = list()
    for(i in 1:length(files)){
      l[[i]] = strsplit(strsplit(files[i],"/")[[1]][7],".csv")
    }
    
    vect = c()
    for(i in 1:length(files)){
      vect[i] = l[[i]][[1]][1]     
    }
    return (vect)
  
}
