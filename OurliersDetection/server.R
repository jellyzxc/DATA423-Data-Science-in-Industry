#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$StructureD1 <- renderPrint({
        str(iris)
    })
    output$SummaryD1 <- renderPrint({
        summary(iris)
    })
    
    output$RawTableD1 <- DT::renderDataTable({
        DT::datatable(
            data = iris,
            rownames = FALSE 
        ) 
    })
    
    
    output$DensityPlot <- renderPlotly({
        
        data_Density=iris[,input$Variables1]    
        title=paste(input$Variables1,"before YJ transform")
        
        if(input$YeoJohnson==TRUE)
        {
            
            title=paste(input$Variables1,"after YJ transform")
            data_Density=yeojohnson(data_Density)$x.t
        }
        
        density <- density(data_Density)
        
        fig <- plot_ly(x = ~density$x, y = ~density$y, type = 'scatter', mode = 'lines', fill = 'tozeroy')
        fig <- fig %>% layout(xaxis = list(title = input$Variables1),
                              yaxis = list(title = 'Density'))
       
    })
    
    output$Boxplot <- renderPlotly({
 
        data_box=iris[,input$Variables2]  
      
        fig <- plot_ly()
        fig <- fig %>% add_boxplot(y = data_box, name =input$Variables2, boxpoints = 'suspectedoutliers',quartilemethod=input$quartilemethod,
                                   marker = list(color = 'rgb(8,81,156)',
                                                 outliercolor = 'rgba(219, 64, 82, 0.6)',
                                                 line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                             outlierwidth = 2)),
                                   line = list(color = 'rgb(8,81,156)'))
       
    }) 
    
    
    
    output$BoxplotTable <- DT::renderDataTable({
      var_sel= iris[,input$Variables2] 
      box=boxplot(var_sel,outline = TRUE,range = 1.5)
      index=NULL;
      for(i in box$out )
      {
        index=append(index,which(var_sel==i))
      }
      data_box=data.frame(index,observation=box$out)
   
      r11$data<<-data_box
 
      DT::datatable(
        data =data_box ,options = list(dom = 'tp'), width="200",
        rownames = FALSE 
      )   
    })
    
    
    
    
    output$outlier <- renderPlot({
        
        var_sel=iris[,input$Variables3] 
        dis=input$distribution
        K <- getOutliers(var_sel,method="I",distribution=dis)
        L <- getOutliers(var_sel,method="II",distribution=dis)
        par(mfrow=c(1,2))
        outlierPlot(var_sel,K,mode="qq")
        outlierPlot(var_sel,L,mode="residual")
        
  
    })
    
    
  
    
    
    output$outlierTableK <- DT::renderDataTable({
        var_sel=iris[,input$Variables3] 
        dis=input$distribution
        K <- getOutliers(var_sel,method="I",distribution=dis)
        R=data.frame(index=K$iRight,observation=var_sel[K$iRight])
        L=data.frame(index=K$iLeft,observation=var_sel[K$iLeft])
        
        outliers=rbind(L,R)
 
        
        r12_1$data<<-outliers
        
        DT::datatable(
            data = outliers,options = list(dom = 'tp'),
            rownames = FALSE 
        )   
        
    })
    output$outlierTableL <- DT::renderDataTable({
        var_sel=iris[,input$Variables3] 
        dis=input$distribution
        L <- getOutliers(var_sel,method="II",distribution=dis)
        R=data.frame(index=L$iRight,observation=var_sel[L$iRight])
        L=data.frame(index=L$iLeft,observation=var_sel[L$iLeft])
        
        outliers=rbind(L,R)
       
        
        r12_2$data<<-outliers
        
        DT::datatable(
            data = outliers,options = list(dom = 't$data'), width="200",
            rownames = FALSE 
        )   
    })
    
    output$OutlierDetectionTable <- renderDataTable({
        
        var_DATA=iris[,input$Variables4] 
        method=input$method
        uu=UnivariateOutlierDetection(var_DATA,cutoff=.95,Method=method,rnames=FALSE)
        data=data.frame(index=uu$`Location of Outlier`, observation=uu$`Outlier Observations`)
      
        r13$data<<-data
        
        DT::datatable(
            data=data,
            options = list(dom = 'tp'),
            rownames = FALSE
        )
    })
    
    
    output$OutlierDetection <- renderPlot({
        
        var_DATA=iris[,input$Variables4] 
        method=input$method
        uu=UnivariateOutlierDetection(var_DATA,cutoff=.95,Method=method,rnames=FALSE)
        uu$`Scatter plot`
    })
    
    
    output$Summary1 <- renderDataTable({
      
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Method'),
            th(colspan = 2, 'Outlier'),
          ),
          tr(
            lapply(c('index', 'observation'), th)
          )
        )
      ))
      print(sketch)

     
      
      sumdata_1=NULL
      if(!is.null(r11$data)){
        sumdata_1=sumdata_1%>%rbind(data.frame(menthod=rep(menthod_1[1],times=nrow(r11$data)),r11$data)) 
      }
      if(!is.null(r12_1$data)){
        sumdata_1=sumdata_1%>%rbind(data.frame(menthod=rep(menthod_1[2],times=nrow(r12_1$data)),r12_1$data) )
      }
      if(!is.null(r12_2$data)){
        sumdata_1=sumdata_1%>%rbind(data.frame(menthod=rep(menthod_1[3],times=nrow(r12_2$data)),r12_2$data) )
      }
      if(!is.null(r13$data)){
        sumdata_1=sumdata_1%>%rbind(data.frame(menthod=rep(menthod_1[4],times=nrow(r13$data)),r13$data)) 
      }

      DT::datatable(
        caption = 'Summary of observations each method identifies as outliers.',
        sumdata_1, container = sketch, rownames = FALSE,options = list(pageLength = 15))  
      
    })
    
 
    
    
    #======only Bivariable
    output$colorplot <- renderPlot({
        
        colorData=iris[,input$Variables21] 
        color.plot(colorData)
    })
    
    output$colorplotTable <- DT::renderDataTable({
        
        colorData=iris[,input$Variables21] 
        cp=color.plot(colorData)
        index=which(cp$outliers==TRUE)
        var=data.frame(index,colorData[index,]) 
        
        r21$data<<-var
        
        DT::datatable(
            data=var, 
            options = list(dom = 'tp'),
            rownames = FALSE 
        )   
        
    })
    
    
    output$ddplot <- renderPlot({
        
        ddData=iris[,input$Variables22] 
        dd.plot(ddData)
    })
    
    output$ddplotTable <- DT::renderDataTable({
        
        ddData=iris[,input$Variables22] 
        dp=dd.plot(ddData)
        index=which(dp$outliers==TRUE)
        var=data.frame(index,ddData[index,]) 
        
        r22$data<<-var
        
        DT::datatable(
            data=var,
            options = list(dom = 'tp'),
            rownames = FALSE 
        )   
        
    })
    
    output$Bagplot <- renderPlot({
        
        bagData=iris[,input$Variables23] 
        plot(bagData, xlab=input$Variables23[1], ylab=input$Variables23[2])
        bagplot(bagData, cex = 0.9,add=TRUE)   #add=TRUE  add  bagplot to an existing plot
        
    })
    output$BagplotTable <- DT::renderDataTable({
        
        bagData=iris[,input$Variables23] 
        bag=bagplot(bagData, cex = 0.9)
 
        indexx=NULL;
        indexy=NULL;
      
        x=bag$pxy.outlier[,1]
        y=bag$pxy.outlier[,2]
        for(i in x )
        {
          indexx=append(indexx,which(bagData[,1]==x))
        }
        
        for(j in y )
        {
          indexy=append(indexy,which(bagData[,2]==y))
        }
        
        index=intersect(indexx,indexy)
        
        temp=data.frame(index,bag$pxy.outlier)
        if(nrow(temp)>0){
          colnames(temp)[2]=input$Variables23[1]
          colnames(temp)[3]=input$Variables23[2]
        }   
      
         
        r23$data<<-temp

        DT::datatable(
            data=temp,
            options = list(dom = 'tp'),
            rownames = FALSE 
        ) 
        
    })
    
    
    getdata <- reactive({
      sumdata=NULL
      if(!is.null(r21$data)){
        sumdata=sumdata%>%rbind(data.frame(menthod=rep(menthod_2[1],times=nrow(r21$data)),r21$data)) 
      }
      if(!is.null(r22$data)){
        sumdata=sumdata%>%rbind(data.frame(menthod=rep(menthod_2[2],times=nrow(r22$data)),r22$data) )
      }
      if(!is.null(r23$data)){
        sumdata=sumdata%>%rbind(data.frame(menthod=rep(menthod_2[3],times=nrow(r23$data)),r23$data)) 
      }
      
    }) 
    
   
    output$Summary2 <- renderDataTable({
 
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Method'),
            th(colspan =3, 'Outlier'),
          ),
          tr(
            lapply(c('index', 'variable1','variable2'), th)
          )
        )
      ))
      print(sketch)

      DT::datatable(
        caption = 'Summary of observations each method identifies as outliers.',
        getdata(), container = sketch, rownames = FALSE,options = list(pageLength = 15))

    })
    
    
    
    
    
    
    
    
    #============Multivariable==============
    output$Mosaic <- renderPlot({
       
        m_data<-titanic 
        formula <- as.formula(paste("~",paste(input$Variables31, collapse = " + ")))
        vcd::mosaic(formula, data = m_data,
                    main = "", shade = TRUE, legend = TRUE)
    })
    
     
    output$mahaplot <- renderPlotly({
        
        mhData=iris[,input$Variables32] 
       
        mh=maha(mhData,cutoff=input$cutoff1)
        mh$`3Dplot`
        
    })
    
    output$mahaTable <- DT::renderDataTable({
        
        mhData=iris[,input$Variables32] 
        mh=maha(mhData,cutoff=input$cutoff1)
        data=data.frame(index=mh$`Location of Outlier`, Prob=mh$`Outlier Probability`,  mh$`Outlier Observations`)
       
        r31$data=data[,1]
        
        DT::datatable(
            data=data,
            options = list(dom = 'tp'),
            rownames = FALSE
        )
        
    })
    output$nnkplot <- renderPlotly({
        
        nnkData=iris[,input$Variables33] 
        nnk=nnk(nnkData,cutoff=input$cutoff2,Method=input$method1)
        nnk$`3Dplot`
        
    })
    
    output$nnkTable <- DT::renderDataTable({
        
        nnkData=iris[,input$Variables33] 
        nnk=nnk(nnkData,cutoff=input$cutoff2,Method=input$method1)
 
        data=data.frame(index=nnk$`Location of Outlier`, Prob=nnk$`Outlier Probability`,  nnk$`Outlier Observations`)
        
        r32$data=data[,1]
        DT::datatable(
            data=data,
            options = list(dom = 'tp'),
            rownames = FALSE
        )
        
        
        
    })

    
    output$pcplot <- renderPlotly({
        
        var_Data=iris[,input$Variables35] 
        var_polt=PCOutlierDetection(var_Data,cutoff=input$cutoff5,Method=input$method5)
        var_polt$`3Dplot`
        
    })
    
    output$pcTable <- DT::renderDataTable({
        
        var_Data=iris[,input$Variables35] 
        var_polt=PCOutlierDetection(var_Data,cutoff=input$cutoff5,Method=input$method5)
        index=var_polt$`Location of Outlier`
     
        data=data.frame(index, var_Data[index,])
        r33$data=data[,1]
        DT::datatable(
            data=data,
            options = list(dom = 'tp'),
            rownames = FALSE
        )
 
        
    })
    
    
    output$dispplot <- renderPlotly({
        
        var_Data=iris[,input$Variables36] 
        var_polt=disp(var_Data,cutoff=input$cutoff6)
        var_polt$`3Dplot`
        
    })
    
    output$dispTable <- DT::renderDataTable({
        
        var_Data=iris[,input$Variables36] 
        var_polt=disp(var_Data,cutoff=input$cutoff6)
        data=data.frame(index=var_polt$`Location of Outlier`,  Prob=var_polt$`Outlier Probability`,var_polt$`Outlier Observations`)
        
        r34$data=data[,1]
        DT::datatable(
            data=data,
            options = list(dom = 'tp'),
            rownames = FALSE
        )
        
        
    })
    
    
    output$densplot <- renderPlotly({
      
      var_Data=iris[,input$Variables37] 
      var_polt=dens(var_Data,cutoff=input$cutoff7,C=input$c)
      var_polt$`3Dplot`
      
    })
    
    output$densTable <- DT::renderDataTable({
      
      var_Data=iris[,input$Variables37] 
      var_polt=dens(var_Data,cutoff=input$cutoff7,C=input$c)
      data=data.frame(index=var_polt$`Location of Outlier`,  Prob=var_polt$`Outlier Probability`,var_polt$`Outlier Observations`)
      r35$data=data[,1]
       DT::datatable(
        data=data,
        options = list(dom = 'tp'),
        rownames = FALSE
      )
 
    })
    
    output$outplot <- renderPlotly({
      
      var_Data=iris[,input$Variables38] 
      var_polt=OutlierDetection(var_Data,cutoff=input$cutoff8,Method =input$method8)
      var_polt$`3Dplot`
      
    })
    
    output$outTable <- DT::renderDataTable({
      
      var_Data=iris[,input$Variables38] 
      var_polt=OutlierDetection(var_Data,cutoff=input$cutoff8,Method =input$method8)
      data=data.frame(index=var_polt$`Location of Outlier`,var_polt$`Outlier Observations`)
      
      r36$data=data[,1]
      DT::datatable(
        data=data,
        options = list(dom = 'tp'),
        rownames = FALSE
      )
      
    })
    
    
    output$depthoutplot <- renderPlotly({
      
      var_Data=iris[,input$Variables34] 
      var_polt=depthout(var_Data,cutoff=input$cutoff3)
      var_polt$`3Dplot`
      
    })
    
    output$depthoutTable <- DT::renderDataTable({
      
      var_Data=iris[,input$Variables34] 
      var_polt=depthout(var_Data,cutoff=input$cutoff3)
      data=data.frame(index=var_polt$`Location of Outlier`, Prob=var_polt$`Outlier Probability`,  var_polt$`Outlier Observations`)
      
      r37$data=data[,1]
      DT::datatable(
        data=data,
        options = list(dom = 'tp'),
        rownames = FALSE
      )
      
    })
    output$O3plot <- renderPlot({
      
      var_Data=iris[,input$Variables39] 
      a0 <- O3prep(var_Data, method=input$method9)
      a1 <- O3plotT(a0) 
      a1$gO3
      
    })
    
    output$O3Table <- DT::renderDataTable({
      
      var_Data=iris[,input$Variables39] 
      a0 <- O3prep(var_Data, method=input$method9)
      a1 <- O3plotT(a0) 
      temp=a1$outsTable%>%select(Case)%>%distinct()

      index=temp[,1] 
      r38$data=index 
     
      data=data.frame(index=temp,var_Data[temp$Case,])
      DT::datatable(
        data=data,
        options = list(dom = 'tp'),
        rownames = FALSE
      )
      
    })
    
    
    
    
    output$Summary3 <- renderDataTable({
      
       
     # browser()
        sumdata_3=data.frame(menthod=rep(menthod_3[1],times=length(r31$data)),Outlierindex=r31$data)%>%
                  rbind(data.frame(menthod=rep(menthod_3[2],times=length(r32$data)),Outlierindex=r32$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[3],times=length(r33$data)),Outlierindex=r33$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[4],times=length(r34$data)),Outlierindex=r34$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[5],times=length(r35$data)),Outlierindex=r35$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[6],times=length(r36$data)),Outlierindex=r36$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[7],times=length(r37$data)),Outlierindex=r37$data))%>%
                  rbind(data.frame(menthod=rep(menthod_3[8],times=length(r38$data)),Outlierindex=r38$data)) 
 

      DT::datatable(
        caption = 'Summary of observations each method identifies as outliers.',
        sumdata_3, rownames = FALSE,options = list(pageLength = 15))  
      
    })
    
    
    
    
    #=============other===========
    
    
    output$knndist <- renderPlot({
      var_Data=iris[,input$Variables42] 
      dbscan::kNNdistplot(var_Data, k = input$K)
      abline(h = input$eps, lty = 3, col="red")
      
    })
    
    output$noisepointsSum <-renderPrint({
      var_Data=iris[,input$Variables42] 
      eps=input$eps
      minPts=input$K
      clustered <- dbscan::dbscan(var_Data, eps = eps, minPts =minPts)
    
      clustered
    })
    
    output$NoiseTable <- DT::renderDataTable({
      var_Data=iris[,input$Variables42] 
      eps=input$eps
      minPts=input$K
      clustered <- dbscan::dbscan(var_Data, eps = eps, minPts =minPts)
 
      index=which(clustered$cluster==0)
      dd<-data.frame(index,var_Data[index,])
      DT::datatable(
        data =dd,# var_Data[clustered$cluster == 0,],
        options = list(dom = 'tp'),
        rownames = FALSE
      ) 
    })
    
    
    output$lofscore <- DT::renderDataTable({
      
      K=input$K3
      lofdata=iris[,input$Variables43]
      
      d <- dbscan::lof(lofdata, k = K)  #numericData
      lofdata$LOF_score <- d
      lofdata <- lofdata[order(d, decreasing = TRUE),]
      
      DT::datatable(
        data = lofdata,
        options = list(dom = 'tp'),
        rownames = FALSE
      ) 
      
      datatable(lofdata) %>%
        formatStyle("LOF_score", backgroundColor = JS("value > 2? 'orange' : ( value > 1? 'yellow':'')"))%>%
        formatRound("LOF_score", 3) 
    })
 
    
    output$lof3D <- renderPlotly({
      lofdata=iris[,input$Variables43]
      K=input$K3
      title=paste( "LOF when k=",K)
      d <- dbscan::lof(lofdata, k = K)
      lofdata$LOF_score <- d
      lofdata <- lofdata[order(d, decreasing = TRUE),]
      plot_ly(lofdata, x = ~lofdata[,1], y = ~lofdata[,2], z = ~lofdata[,3], color = ~LOF_score,  
              marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
              text = ~paste('LOF_score:', LOF_score)) 
               
    })
    
    
    output$svm <- DT::renderDataTable({
      
      svmdata=iris[,input$Variables44]
      model <- e1071::svm(svmdata, y = NULL, type = 'one-classification', nu = 0.10, scale = TRUE, kernel = input$kernal)
      result <- predict(model, svmdata)
      
      index=which(!result)
      svmdata=data.frame(index,svmdata[index,])
 
      DT::datatable(
        data = svmdata,
        options = list(dom = 'tp'),
        rownames = FALSE
      ) 
      
    })
    
    
    output$forestTable <- DT::renderDataTable({
      ntrees=input$ntree
      ndim=input$ndim
      forestData=iris[,input$Variables45]
      iso <- isolation.forest(forestData, ntrees = ntrees, ndim = ndim)
      score <- predict(iso, forestData,type = "score")
      forestData$Score <- score
      forestData <- forestData[order(score, decreasing = TRUE),]
      DT::datatable(
        data = forestData,options = list(dom = 'tp'),
        rownames = FALSE
 
      ) 
      
      datatable(forestData) %>%
        formatStyle("Score", backgroundColor = JS("value > 1? 'orange' : ( value > 0.5? 'yellow':'')"))%>%
        formatRound("Score", 3)
    })
    
    output$forestPlot <- renderPlotly({
      
      
      ntrees=input$ntree
      ndim=input$ndim
      forestData=iris[,input$Variables45]
      iso <- isolation.forest(forestData, ntrees = ntrees, ndim = ndim)
      score <- predict(iso, forestData,type = "score")
      forestData$Score <- score
      
      plot_ly(forestData, x = ~forestData[,1], y = ~forestData[,2], z = ~forestData[,3],  color = ~Score,
              marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
              text = ~paste('Score:', Score))  

    })
    
    
    #=========cook ==============
    output$cook <- renderPlotly({
      
      threshold_var=input$threshold 
      glmfit = glm(Sepal.Length ~Sepal.Width+Petal.Length+Petal.Width+Species, family = gaussian, data = iris)
      dc <- cooks.distance(glmfit)
      thresh <- threshold_var * mean(dc)
      cookdata<-data.frame(cooks.distance=dc,Obs.number=1:150)
     
      plot_ly(x = ~Obs.number, y = ~cooks.distance,  type = 'scatter',mode = 'markers',name ='cooks.distance', data = cookdata)%>%
          add_segments(y = thresh, yend = thresh, x = 0,xend = 150,name = 'threshold') %>% layout(legend = list(x = 0.1, y = 0.9)) 
      
    })
    
    output$cooktable <- DT::renderDataTable({
      threshold_var=input$threshold 
      glmfit = glm(Sepal.Length ~Sepal.Width+Petal.Length+Petal.Width+Species, family = gaussian, data = iris)
      dc <- cooks.distance(glmfit)
      thresh <- threshold_var * mean(dc)
      cooktData=data.frame(index=which(dc>thresh), iris[which(dc>thresh),])
      DT::datatable(
        data = cooktData,options = list(dom = 'tp'),
        rownames = FALSE
        
      ) 
      
    })
    
    
    #===========final============
    output$kftable <- DT::renderDataTable({
    
      irisM <-iris[,input$Variables51]
      folds = createFolds(1:150, k = input$KFold )
      cv = lapply(folds, function(x) {  
        
        training_fold = irisM[-x, ] # training fold  
        test_fold = irisM[x, ]  # testing fold  
        
        classifier = svm(training_fold,y = NULL, 
                         type = 'one-classification',
                         nu = 0.10, scale = TRUE,
                         kernel = input$kernal5)
        
        y_pred = predict(classifier, newdata = test_fold)
        
        result=data.frame(index=x,result=y_pred)
        
        return(result[!result$result,])   #the outliers in the current test fold
      })
      
      indx=NULL;
      for(temp in cv){
        indx=rbind(indx,temp) 
      }
      
      indx=sort(indx$index,decreasing = FALSE)
      final=data.frame(indx,irisM[indx,])
    
      DT::datatable(
        data = final, options = list(dom = 'tp'),
        rownames = FALSE
        
      ) 
      
    })
    
    
    output$svm2 <- DT::renderDataTable({
      
      svmdata=iris[,input$Variables51]
      model <- e1071::svm(svmdata, y = NULL, type = 'one-classification', nu = 0.10, scale = TRUE,  kernel = input$kernal5)
      result <- predict(model, svmdata)
      index=which(!result)
      rr=data.frame(index,svmdata[index,])
      
      DT::datatable(
        data = rr,
        options = list(dom = 'tp'),
        rownames = FALSE
      ) 
      
    })
    
})
