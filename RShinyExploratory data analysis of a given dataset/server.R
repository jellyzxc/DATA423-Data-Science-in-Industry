#author:XiaoxuiZhang 16142030
#423Assignment1  2020-03-04

## Serve
shinyServer(function(input, output) {
    
    # ----------Summary  
    #raw data
    output$SummaryR1 <- renderPrint({
      # glimpse(newdata)
       str(newdata)
      
    })
    output$SummaryR2 <- renderPrint({
        summary(newdata)
    })
     
    output$SummaryCate <- renderPrint({
        summary(Qualitative_data)
    })
    output$SummaryOrd<- renderPrint({
        summary(Ordinal_data)
    })
    output$SummaryCont <- renderPrint({
        summary(Quantitative_data)
    })
  
    output$SummaryDate <- renderPrint({
        summary(Date_data)
    })
    
 
    
    # ----------Visualisation
    #detect NA
    output$Missing <- renderPlot({
      data_miss <- newdata[,input$VariablesC]  
      vis_miss(data_miss, sort_miss = FALSE, show_perc = TRUE, show_perc_col = TRUE,cluster = input$cluster,warn_large_data=TRUE) +
        labs(title = "Missing values distribution")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
    })
    
    
    #find something abnormal
    output$Mosaic <- renderPlot({
        m_data<-Qualitative_data%>%na.omit()
        formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
        vcd::mosaic(formula, data = m_data,
                    main = "This is a mosaic plot of factors", shade = TRUE, legend = TRUE)
    })
    
    output$Boxplot <- renderPlot({
      data <- Quantitative_data[,input$VariablesB]  
      data <- scale(data, center = input$standardise, scale = input$standardise)
      boxplot(x = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
              horizontal = FALSE, outline = input$outliers, 
              col = brewer.pal(n = ncol(data), name = "RdBu"),
              range = input$range, main = "Boxplots of numerical data")
    })
    
     
    output$Corrgram <- renderPlot({
 
        data_cor <- Quantitative_data[,input$VariablesD] 
        corrgram(data_cor,  order = "OLO",  
                 abs = input$abs,
                 #text.panel = panel.txt,
                 main = "Correlation of all numeric data ",
                 lower.panel=panel.shade,
                 upper.panel=panel.shade, 
                 diag.panel=panel.minmax)
    })
    #panel.bar, panel.minmax,panel.density  panel.conf  panel.ellipse    order=TRUE,   
   
    
    output$Corrgram1 <- renderPlot({
      corrgram(q1, #order = "OLO",   
               text.panel = panel.txt,
               main = "Correlation of Group1 ",
               lower.panel=panel.shade,
               upper.panel=panel.ellipse 
               )
    })
    output$Corrgram2 <- renderPlot({
      corrgram(q2, #order = "OLO",  
               text.panel = panel.txt,
               main = "Correlation of Group2 ",
               lower.panel=panel.shade,
               upper.panel=panel.density 
      )
    })                                                                            
    output$Corrgram3 <- renderPlot({
      corrgram(q3, #order = "OLO", 
               text.panel = panel.txt,
               main = "Correlation of Group3 ",
               lower.panel=panel.shade,
               upper.panel=panel.cor 
      )
    })
    output$Corrgram4 <- renderPlot({
      corrgram(q4, #order = "OLO",
               text.panel = panel.txt,
               main = "Correlation of Group4 ",
               lower.panel=panel.shade,
               upper.panel=panel.pie 
      )
    })
    
    
    output$Pairs1 <- renderPlot({
      GGally::ggpairs(data = q1, title = "Pairs of Group1")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
    })
    output$Pairs2 <- renderPlot({
      GGally::ggpairs(data = q2, title = "Pairs of Group2")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
    })                                                                            
    output$Pairs3 <- renderPlot({
      GGally::ggpairs(data = q3, title = "Pairs of Group3")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
    }) 
    output$Pairs4 <- renderPlot({
      GGally::ggpairs(data = q4, title = "Pairs of Group4")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
    }) 
    
 
    
    output$MixedPairs <- renderPlot({
      
      data_mix<-newdata[,c(input$VariablesF,input$VariablesE)]
      data_mix<-data_mix%>%na.omit()
      ct<-data_mix[,input$VariablesE]
      GGally::ggpairs(data = data_mix,  mapping = ggplot2::aes(colour=unlist(ct)),title = "Pairs of mixed variables")+
        theme(plot.title = element_text(hjust = 0.5,size=14, face='bold'))
      
    })
    
     output$Continuity <- renderPlot({
      data_Continuity <- Quantitative_data[,input$VariablesI] 
    
      for (col in 1:ncol(data_Continuity)) {
        data_Continuity[,col] <- data_Continuity[order(data_Continuity[,col]),col] #sort each column in ascending order
      }
      
      my_data_Continuity<-data_Continuity
      
      if(input$scale==TRUE )
      {
        my_data_Continuity<- scale(x = data_Continuity, center = TRUE, scale = TRUE)  # scale so they can be graphed with a shared Y axis
       
       }
      
      mypalette <- rainbow(ncol(my_data_Continuity))
      matplot(y = my_data_Continuity, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1,
              col = mypalette, main = "Rising Order chart")
      
      legend(legend = colnames(my_data_Continuity), x = "topleft", y = "top", lty = 1, lwd = 1, 
             col = mypalette, 
             ncol = round(ncol(my_data_Continuity)^0.3))
      
    })
    
    output$Homogeneity <- renderPlot({
      data_Homogeneity<- Quantitative_data[,input$VariablesJ] 
      data_Homogeneity <- scale(data_Homogeneity, center = TRUE, scale = TRUE) # Normalise so they can share a common y axis
      mypalette2<- rainbow(ncol(data_Homogeneity))
      matplot(data_Homogeneity, type = "l", col = alpha(mypalette2, 0.4))  #alpha :to calculate a Cronbach's alpha
      legend(legend = colnames(data_Homogeneity), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette2,ncol = round(ncol(data_Homogeneity)^0.3))
    })
    
 
    output$TimeSeries <- renderPlot({
       td<- newdata[,input$VariablesT] 
       Series <- ts(td , start = c(2001,01,07), frequency = 52)
       plot(Series, main="The trend of the selected varaibles") 
    })
    
    
    # ----------display datatable
    output$rawdata <- DT::renderDataTable({
      rawd_data<-newdata[,input$VariablesH]
      DT::datatable(
        data = rawd_data,
        rownames = FALSE,
        selection = input$selection,
        filter = list(position = input$filter),
        options = list(
          searching = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 100),
          dom = paste(input$dom, collapse = ""),
          ordering = input$order
          ) 
        ) 
      
    })
})
    
 
