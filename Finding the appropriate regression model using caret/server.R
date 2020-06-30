shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ##################################------Resampling methods---trainControl--------############################################ 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  # number: number of resampling iterations
  # index: a list with elements for each resampling iteration ,this is unlikely to be the case when parallel processing is used
  
  ##############################################################################
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })

  ##############################################################################
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })

  ##############################################################################
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", text.panel = panel.txt,main = "Numeric Data Correlation"   #, lower.panel=panel.shade, upper.panel=panel.cor
                       )
  })

  # ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })

  # ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })

  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ##############################################################################
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })

  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  

  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)   #models[[method]] 
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  

  
  
######################################################### maintenance point ####################################################
  

  
  ####################### lm #######################################################
  output$lmModelSummary0 <- renderText({
    description("lm")
  })
  
  getlmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$lmPreprocess)
  }) 
  
  observeEvent(
    input$lmGo,
    {
      library(stats)
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$lmMetrics <- renderTable({
    req(models$lm) 
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  
  output$lmRecipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  output$lmModelPlot <- renderPlot({
    req(models$lm)
    par(mfrow = c(2, 2))
    plot(models$lm$finalModel)
  })  
  output$lmModelSummary2 <- renderPrint({
    req(models$lm)
    
    print(models$lm)
  })
  
 
  
  
  
  ############################# rlm #######################################################
  output$rlmModelDiscription <- renderText({
    description("rlm")
  })
  
  getrlmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rlmPreprocess)
  }) 
  
  observeEvent(
    input$rlmGo,
    {
      library(stats)
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getrlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$rlmMetrics <- renderTable({
    req(models$rlm) 
 
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  output$rlmModelPlot <- renderPlot({
    req(models$lm)
    par(mfrow = c(2, 2))
    plot(models$rlm$finalModel)
  })  
  
  output$rlmRecipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  output$rlmModelSummary2 <- renderPrint({
    req(models$rlm)
    print(models$rlm)
  })
  
  output$rlmModelSummary1 <- renderPrint({
    req(models$rlm)
    print(models$rlm$finalModel)
  })
  
  
  
  ######################## lmStepAIC #######################################################
  output$lmStepAICModelSummary0 <- renderText({
    description("lmStepAIC")
  })
  
  getlmStepAICRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$lmStepAICPreprocess)
  }) 
  
  observeEvent(
    input$lmStepAICGo,
    {
      library(stats)
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getlmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$lmStepAICMetrics <- renderTable({
    req(models$lmStepAIC) 
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  
  output$lmStepAICRecipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })  
  
  output$lmStepAICModelPlot <- renderPlot({
    req(models$lmStepAIC)
    par(mfrow = c(2, 2))
    plot(models$lmStepAIC$finalModel)
  })  
  output$lmStepAICModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    
    print(models$lmStepAIC)
  })
  
  
  
  ################ GLM #######################################################
  output$bayesglmModelSummary0 <- renderText({
    description("glm")
  })
  
  getglmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmPreprocess)
  }) 
  
  observeEvent(
    input$glmGo,
    {
      library(arm)
      method <- "glm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$glmMetrics <- renderTable({
    req(models$glm) 
    models$glm$results[ which.min(models$glm$results[, "RMSE"]), ]
  })
  
  
  output$glmRecipe <- renderPrint({
    req(models$glm)
    
    models$glm$recipe
  })  
  
  
  output$glmModelSummary2 <- renderPrint({
    req(models$glm)
    print(models$glm)
  })
  
   
  
  
  ################ BayesGLM #######################################################
  output$bayesglmModelSummary0 <- renderText({
    description("bayesglm")
  })
  
  getbayesglmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bayesglmPreprocess)
  }) 
  
  observeEvent(
    input$bayesglmGo,
    {
      library(arm)
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbayesglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$bayesglmMetrics <- renderTable({
    req(models$bayesglm) 
    models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  })
  
  
  output$bayesglmRecipe <- renderPrint({
    req(models$bayesglm)
 
    models$bayesglm$recipe
  })  
  
  output$bayesglmModelSummary2 <- renderPrint({
    req(models$bayesglm)
    print(models$bayesglm)
  })
  
 
  ########################################  svmLinear2    #############################  
  getSVMLinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SVMLinearPreprocess)
  })
  
  
  observeEvent(
    input$SVMLinearGo,
    {
      library(e1071)
      method <- "svmLinear2"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getSVMLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$SVMLinearSummary0 <- renderText({
    description("svmLinear2")
  })
  
  
  output$SVMLinearMetrics <- renderTable({
    req(models$svmLinear2)   #models[[method]] 
    models$svmLinear2$results[ which.min(models$svmLinear2$results[, "RMSE"]), ]
  })
  
  output$SVMLinearPlots <- renderPlot({
    req(models$svmLinear2)
    plot(models$svmLinear2)
  })
  
  
  output$SVMLinearRecipe <- renderPrint({
    req(models$svmLinear2)
    models$svmLinear2$recipe
  })  
  
  output$SVMLinearSummary2 <- renderPrint({
    req(models$svmLinear2)
    print(models$svmLinear2)
  })
  
  
  #################################################      SVMPoly       #############################  
  getsvmPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmPolyPreprocess)
  })
  
  
  observeEvent(
    input$svmPolyGo,
    {
      library(kernlab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$svmPolySummary0 <- renderText({
    description("svmPoly")
  })
  
  
  output$svmPolyMetrics <- renderTable({
    req(models$svmPoly)    
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  output$svmPolyPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  
  output$svmPolyRecipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  output$svmPolySummary2 <- renderPrint({
    req(models$svmPoly)
    print(models$svmPoly)
  })
  
  
  
  #################################################      svmRadial       #############################  
  getsvmRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmRadialPreprocess)
  })
  
  
  observeEvent(
    input$svmRadialGo,
    {
      library(kernlab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$svmRadialSummary0 <- renderText({
    description("svmRadial")
  })
  
  
  output$svmRadialMetrics <- renderTable({
    req(models$svmRadial)    
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  output$svmRadialPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })
  
  
  output$svmRadialRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  output$svmRadialSummary2 <- renderPrint({
    req(models$svmRadial)
    print(models$svmRadial)
  })
  
  
  #################################################      gaussprPoly       #############################  
  getgaussprPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprPolyPreprocess)
  })
  
  
  observeEvent(
    input$gaussprPolyGo,
    {
      library(kernlab)
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$gaussprPolySummary0 <- renderText({
    description("gaussprPoly")
  })
  
  
  output$gaussprPolyMetrics <- renderTable({
    req(models$gaussprPoly)    
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  output$gaussprPolyPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })
  
  
  output$gaussprPolyRecipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  output$gaussprPolySummary2 <- renderPrint({
    req(models$gaussprPoly)
    print(models$gaussprPoly)
  })
  
  
  ##########################   krlsPoly ####################################################
  
  getkrlsPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$krlsPolyPreprocess)
  })
  
  
  observeEvent(
    input$krlsPolyGo,
    {
      library(KRLS)
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "krlsPoly")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$krlsPolyModelSummary0 <- renderText({
    description("krlsPoly")
  })
  
  
  output$krlsPolyMetrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  
  output$krlsPolyRecipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  
  output$krlsPolyModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  output$krlsPolyModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    print(models$krlsPoly)
  })
  
  
  ##########################   GAM ####################################################
  
  getgamRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gamPreprocess)
  })
  
  
  observeEvent(
    input$gamGo,
    {
      library(mgcv)
      
      method <- "gam"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgamRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "gam")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$gamModelSummary0 <- renderText({
    description("gam")
  })
  
  
  output$gamMetrics <- renderTable({
    req(models$gam)
    models$gam$results[ which.min(models$gam$results[, "RMSE"]), ]
  })
  
  
  output$gamRecipe <- renderPrint({
    req(models$gam)
    models$gam$recipe
  })  
  
  
  output$gamModelPlots <- renderPlot({
    req(models$gam)
    plot(models$gam)
  })
  
  output$gamModelSummary2 <- renderPrint({
    req(models$gam)
    print(models$gam)
  })  
  
  
  
  
  
  
  
  
  ##########################   gamboost ####################################################
  
  getgamboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gamboostPreprocess)
  })
  
  
  observeEvent(
    input$gamboostGo,
    {
      library(mboost)  
      library(plyr) 
      library(import) 
      method <- "gamboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgamboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "gamboost")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$gamboostModelSummary0 <- renderText({
    description("gamboost")
  })
  
  
  output$gamboostMetrics <- renderTable({
    req(models$gamboost)
    models$gamboost$results[ which.min(models$gamboost$results[, "RMSE"]), ]
  })
  
  
  output$gamboostRecipe <- renderPrint({
    req(models$gamboost)
    models$gamboost$recipe
  })  
  
  
  output$gamboostModelPlots <- renderPlot({
    req(models$gamboost)
    plot(models$gamboost)
  })
  
  output$gamboostModelSummary2 <- renderPrint({
    req(models$gamboost)
    print(models$gamboost)
  })  
  
  
  
  ##########################   earth ####################################################
  
  getearthRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$earthPreprocess)
  })
  
  
  observeEvent(
    input$earthGo,
    {
      library(earth)  
      method <- "earth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getearthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "earth")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$earthModelSummary0 <- renderText({
    description("earth")
  })
  
  
  output$earthMetrics <- renderTable({
    req(models$earth)
    models$earth$results[ which.min(models$earth$results[, "RMSE"]), ]
  })
  
  
  output$earthRecipe <- renderPrint({
    req(models$earth)
    models$earth$recipe
  })  
  
  
  output$earthModelPlots <- renderPlot({
    req(models$earth)
    plot(models$earth)
  })
  
  output$earthModelSummary2 <- renderPrint({
    req(models$earth)
    print(models$earth)
  })  
  
  
  
  
  
  
  
  ##########################   gcvEarth ####################################################
  
  getgcvEarthRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gcvEarthPreprocess)
  })
  
  
  observeEvent(
    input$gcvEarthGo,
    {
      library(earth)
      
      method <- "gcvEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgcvEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "gcvEarth")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$gcvEarthModelSummary0 <- renderText({
    description("gcvEarth")
  })
  
  
  output$gcvEarthMetrics <- renderTable({
    req(models$gcvEarth)
    models$gcvEarth$results[ which.min(models$gcvEarth$results[, "RMSE"]), ]
  })
  
  
  output$gcvEarthRecipe <- renderPrint({
    req(models$gcvEarth)
    models$gcvEarth$recipe
  })  
  

  output$gcvEarthModelSummary2 <- renderPrint({
    req(models$gcvEarth)
    print(models$gcvEarth)
  })  
  
  
  
  ########################## Blackboost ####################################################
  
  getblackboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$blackboostPreprocess)
  })
  
  
  observeEvent(
    input$blackboostGo,
    {
      library(party)
      library(mboost)
      library(plyr)
      library(partykit) 
      
      method <- "blackboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getblackboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "blackboost")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$blackboostModelSummary0 <- renderText({
    description("blackboost")
  })
  
  
  output$blackboostMetrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  
  output$blackboostRecipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })  
  
  
  output$blackboostModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })
  
  output$blackboostSummary2 <- renderPrint({
    req(models$blackboost)
    print(models$blackboost)
  })
  
  
 
  
  ##########################   RRF ####################################################
  
  getRRFRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RRFPreprocess)
  })
  
 
  observeEvent(
    input$RRFGo,
    {
      library(randomForest)
      library(RRF)
      method <- "RRF"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRRFRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "RRF")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$RRFModelSummary0 <- renderText({
    description("RRF")
  })
  
  
  output$RRFMetrics <- renderTable({
    req(models$RRF)
    models$RRF$results[ which.min(models$RRF$results[, "RMSE"]), ]
  })
  
  
  output$RRFRecipe <- renderPrint({
    req(models$RRF)
    models$RRF$recipe
  })  
  
  
  output$RRFModelPlots <- renderPlot({
    req(models$RRF)
    plot(models$RRF)
  })
  output$RRFModelimprtance <- renderPlot({
     library(RRF)
     req(models$RRF)
     varImpPlot(models$RRF$finalModel)
  })
  
 
  
  output$RRFSummary2 <- renderPrint({
    req(models$RRF)
    print(models$RRF)
  })
  
   
 
  ##########################   QRF ####################################################
  
  getqrfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$qrfPreprocess)
  })
  
  
  observeEvent(
    input$qrfGo,
    {
      library(quantregForest)
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "qrf")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$qrfModelSummary0 <- renderText({
    description("qrf")
  })
  
  
  output$qrfMetrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  
  output$qrfRecipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  
  output$qrfModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  
  output$qrfModelimprtance <- renderPlot({
    req(models$qrf)
    library(randomForest)
    randomForest::varImpPlot(models$qrf$finalModel)
    
  })
  
  output$qrfSummary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
   
 
  # ##########################   XgbTree   -abandon ####################################################
  # 
  # getxgbTreeRecipe <- reactive({
  #   recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xgbTreePreprocess)
  # })
  # 
  # 
  # observeEvent(
  #   input$xgbTreeGo,
  #   {
  #     library(xgboost) 
  #     library(plyr) 
  #     method <- "xgbTree"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       models[[method]] <- caret::train(getxgbTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
  #       saveToRds(models[[method]], "xgbTree")
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # 
  # output$xgbTreeModelSummary0 <- renderText({
  #   description("xgbTree")
  # })
  # 
  # 
  # output$xgbTreeMetrics <- renderTable({
  #   req(models$xgbTree)
  #   models$xgbTree$results[ which.min(models$xgbTree$results[, "RMSE"]), ]
  # })
  # 
  # 
  # output$xgbTreeRecipe <- renderPrint({
  #   req(models$xgbTree)
  #   models$xgbTree$recipe
  # })  
  # 
  # 
  # output$xgbTreeModelPlots <- renderPlot({
  #   req(models$xgbTree)
  #   plot(models$xgbTree)
  # })
  # 
  # 
  # output$xgbTreeSummary2 <- renderPrint({
  #   req(models$xgbTree)
  #   print(models$xgbTree)
  # })
  
   
  ########################## knn ####################################################
  
  getknnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$knnPreprocess)
  })
  
  
  observeEvent(
    input$knnGo,
    {
      
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "knn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
   
  
  output$knnModelSummary0 <- renderText({
    description("knn")
  })
  
  
  output$knnMetrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  
  output$knnRecipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  
  output$knnModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })
  
  output$knnModelSummary2 <- renderPrint({
    req(models$knn)
    print(models$knn)
  })
  
  
  
  ##########################   cubist ####################################################
  
  getcubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$cubistPreprocess)
  })
  
  
  observeEvent(
    input$cubistGo,
    {
      library(Cubist)
      
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], "cubist")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  output$cubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  
  output$cubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  
  output$cubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  
  output$cubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  output$cubistModelPlot2 <- renderPlot({
    req(models$cubist)
    library(Cubist)
    dotplot(models$cubist$finalModel,what = "splits",committee=69)
  })
  
  output$cubistModelSummary1 <- renderPrint({
    req(models$cubist)
    library(Cubist)
    summary(models$cubist$finalModel)
    # Cubist::summary(models$cubist$finalModel)
  })
  
  output$cubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  
 
  
  
  #################################################      neuralnet       #############################  
  getneuralnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$neuralnetPreprocess)
  })
  
  
  observeEvent(
    input$neuralnetGo,
    {
      library(neuralnet)
      method <- "neuralnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getneuralnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 30)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$neuralnetSummary0 <- renderText({
    description("neuralnet")
  })
  
  
  output$neuralnetMetrics <- renderTable({
    req(models$neuralnet)    
  
    models$neuralnet$results[ which.min(models$neuralnet$results[, "RMSE"]), ]
  })
  
  output$neuralnetPlots1 <- renderPlot({
    req(models$neuralnet)
    plot(models$neuralnet)
  
  })
  output$neuralnetPlots2 <- renderPlot({
    req(models$neuralnet)
    plot(models$neuralnet$finalModel)
  })
 
  
  
  output$neuralnetRecipe <- renderPrint({
    req(models$neuralnet)
    models$neuralnet$recipe
  })  
  
  output$neuralnetSummary2 <- renderPrint({
    req(models$neuralnet)
    print(models$neuralnet)
  })
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
   
    NullModel <- "null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models,selected=c("gaussprPoly"))
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    mod$level=factor(mod$metrics)
    bwplot(mod, by=mod$level, notch = input$Notch,plot.order = c(2, 1, 3))
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
 
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
