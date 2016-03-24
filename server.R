library(shiny)
source("listfiles.R")
source("runner.R")
source("polynomialplot.R")
source("stlplot.R")
source("actualplot.R")
source("alllines.R")

server <- function(input,output){

    #THESE TWO LINES RENDER THE DROP DOWN LIST DYNAMICALLY
    ########################################################################
    stock_names <- reactive({
      return(listfiles())
    })
    output$stocknames <- renderUI({
      selectInput("choices","Select your choice",choices = stock_names())
    })
    ########################################################################
    
    
    #THIS IS THE MOTHER OF ALL REACTIVE CALCULATIONS THAT WE SHALL BE DOING
    #THE FUNDAMENTAL OUTPUT IS THE CHOICE OF ON WHICH STOCK TO WORK
    ########################################################################  
    best_predict <- reactive({
      input$choices
    })
    ########################################################################  
    
    
    #THIS EVENT LISTENS FOR THE BUTTON PRESS AND THE GIVES THE PREDICTED RESULT  
    ########################################################################  
    predicted_winner <- eventReactive(input$calculate,{
      return(runner(best_predict()))
    })
    ########################################################################  
    
    #PRINTING THE WINNING MODEL
    ########################################################################  
    output$winner <- renderPrint({
      paste("Winner for ",best_predict()," is ",predicted_winner()[1],sep = "")
    })
    ########################################################################    
    
    
    #PRINTING THE WINNING MODEL
    ########################################################################  
    output$error <- renderPrint({
      paste("Minimum error in the prediction for ",best_predict()," is ",predicted_winner()[2],sep = "")
    })
    ########################################################################   
    
    
    #THESE LINES PRINT THE CURRENTLY SELECTED INPUT(MAY BE REMOVED)
    ########################################################################
    output$text <- renderPrint({
      paste("Now calculating all the predictions for",best_predict(),sep=" ")
    })
    ########################################################################
    
    
    #THE FOLLOWING LINES RENDER A PLOT TO THE APP
    ########################################################################      
    output$polytrend <- renderPlot({
      return(polynomialplot(best_predict()))
    })
    
    
    ########################################################################
    
    #THE FOLLOWING LINES RENDER A PLOT TO THE APP
    ########################################################################      
    output$stltrend <- renderPlot({
      return(stlplot(best_predict()))
    })
    ########################################################################
    
    #THE FOLLOWING LINES RENDER A PLOT TO THE APP
    ########################################################################      
    output$actualtrend <- renderPlot({
      return(actualplot(best_predict()))
    })
    ########################################################################
    
    #THE FOLLOWING LINES RENDER A PLOT TO THE APP
    ########################################################################      
    output$alllinestrend <- renderPlot({
      return(alllines(best_predict()))
    })
    ########################################################################
    
    #********************************************************
    output$display <- reactive({
      if(input$tabs == "Polynomial")
      {
        output$disp <- renderText({
          paste(h3("Polynomial Trending"),
                p("A type of trend that represents a large set of data with many fluctuations.
                            As more data becomes available, trends often become less linear and a polynomial trend takes its place. 
                            Graphs with curved trendlines are generally used to show a polynomial trend.
                           ")
                )
        })
      }
      else if(input$tabs == "STL")
      {
        output$disp <- renderText({
          paste( h3("STL - A Seasonal-Trend Decomposition Procedure Based On Loess"),
                 p("LOESS and LOWESS (locally weighted scatterplot smoothing) are two strongly related non-parametric regression methods that combine multiple regression models in a k-nearest-neighbor-based meta-model.
                            LOESS is a later generalization of LOWESS; although it is not a true initialism, it may be understood as standing for LOcal regrESSion."),
                 p("STL is a filtering procedure for decomposing a time series 
                           into trend,seasonal & remainder components.
                            STL has a simple design that consists of a sequence of applications of the loess smoother;
                           the simplicity allows analysis of the properties of the procedure & allows fast computation,
                           even for very long time series & large amounts of trend & seasonal smoothing."))
        })
      }
      else if(input$tabs == "Actual")
      {
        output$disp <- renderText({
          paste(h3("Actual Plot"),
                p("The actual plot represents variation of stock price with respect to time."))
        })
      }
      else if(input$tabs == "All")
      {
        output$disp <- renderText({
          paste(h3("All Predictions"))
        })
        
      }
      else if(input$tabs == "Information")
      {
        output$disp <- renderText({
          paste(h3("Info"))
        })
      }
      #return(output$display)
    })
}

