function(input, output, session) {
  

  
  riskfree = reactive({  
    df = as.numeric(input$riskfree) 
    return(df)  
  
  
  })
  
  
  
  
 tickers = reactive({  
   tickers = input$variable
   return(tickers)  
   
   
 })
  

   
   
   download = reactive({
     
     #tickers = c("MMM", "ABT", "ABBV", "ABMD", "ACN")
     
     download = as.data.frame(prices())
     
     #download$Date = row.names(download)
     
     return(download)
     
   } )
    
    #####################3 optimization
    
    
    
    
    
    
    
    weights = reactive({  
      
      fx = input$variable
      
      
      
      fx2 = paste0("USD/", fx)
      
       data2 = getFX(fx2,
            from = Sys.Date() -179, to = Sys.Date(),
            #env = .GlobalEnv,
            verbose = FALSE,
            warning = TRUE,
            auto.assign=FALSE
            ) 
     
      
    
      
      
      
      data2 = as.data.frame(data2)
      
      
      colnames(data2)[1] = "Rate"
      
      data2$Date = row.names(data2)
      
      data2$Forecast = "Historical"
      
      return(data2)
      
    
  })
    
    output$downloadData2 <- downloadHandler(
      
      
      filename = function() { 'Returns.csv' }, content = function(file) {
        write.csv(weights(), file, row.names = F)}
      
    )
  
   
    
    
   dataframe = reactive({  
     
     data2 = weights()
     x = data2[nrow(data2), 1]
     
     z = STDEV(data2$Rate)
     
     n = riskfree()
     
     list = rnorm(n, mean = x, sd = z)
     
     
     
     dataframe = as.data.frame(list)
     
     colnames(dataframe)[1] = "Random_Walk"
     
     
     dataframe$Date = Sys.Date() + 1
     
     
     
     dataframe = dataframe %>% group_by(Date) %>% mutate(Random_Walk = mean(Random_Walk)) %>% distinct()
     
     
     mean = dataframe$Random_Walk
     
     
   
       
       
       for (i in c(2:30)) {
         
         list = rnorm(n, mean = mean, sd = z)
         
         df = as.data.frame(list)
         colnames(df)[1] = "Random_Walk"
         df$Date = Sys.Date() + i
         
         mean = df$Random_Walk
         
         df = df %>% group_by(Date) %>% mutate(Random_Walk = mean(Random_Walk)) %>% distinct()
         
         
         dataframe = rbind(df, dataframe)
         
         
       }
       
       return(dataframe)
       
       
    
     
     
   
      
      
    })
  

  
  #############
  
  output$table1 <- renderFormattable({
    
    from <- c( "USD")
    to <- input$variable
    df = getQuote(paste0(from, to, "=X"))
    
    df$Volume =  NULL
    
    
    formattable(df, list(title="Today's Exchange Rate",
                              Expected_Return = color_tile("#ccffcc", "#ccffcc")))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Combine the selected variables into a new data frame
  output$plot1 <- 
    renderDygraph({
    
      
      
     
      
      data2 = weights()
      
      dataframe = dataframe()
    
      
      
      
     
      
      
      
      
      data3 = data2 %>% dplyr::select(Date, Rate)
      
      
      data3 = data3 %>% group_by(Date) %>% mutate(Rate = mean(Rate)) %>% distinct()
      
      dataframe$Date = as.Date(dataframe$Date)
      
      dataframe = xts(dataframe, order.by = dataframe$Date)
      
      data3$Date = as.Date(data3$Date)
      
      data3 = xts(data3, order.by = data3$Date)
      
      
      data4 <- cbind(data3, dataframe)
      
      
      
      
      
  
      
     fig = dygraph(data4) %>%
        #dyLegend(width = 0)   %>%
        dyShading(from = -5000000, to = 5000000, axis = "y", "#ccffcc") %>%
        
        dyShading(from = Sys.Date(), to = Sys.Date() + 31, color = "#ffebe6")%>%
        
        dyRangeSelector()
    fig
  
      
  })
  
  
  
  
  
  
  

  
  
 
  output$minbox <- renderValueBox({
    
    
    dataframe = dataframe()
    
    min = round(min(dataframe$Random_Walk),4)
    
    stdev = STDEV(dataframe$Random_Walk)
    
    
    valueBox(paste0(min), 
             "Forecasted 30-Day Low", icon = icon(""), color = "red")
  }) 
  
  
  
  output$riskfreebox <- renderValueBox({
    
    dataframe = dataframe()
    
    mean = round(mean(dataframe$Random_Walk),4)

    
    
    valueBox(paste0( mean  ), 
             "Forecasted 30-Day Avg.", icon = icon(""), color = "black")
  }) 
  
 
  output$maxbox <- renderValueBox({
    
    dataframe = dataframe()
    
    max = round(max(dataframe$Random_Walk),4)
    

    
    
   valueBox(paste0(max), 
             "Forecasted 30-Day High", icon = icon(""), color = 	"green")
  })  
  
  
  

    
    
    
    
    
    
    
 
    
    
    
    
    
    
    

  
  
  
    
 
  
  
  
}
