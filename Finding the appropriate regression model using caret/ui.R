shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - XiaocuiZhang 16142030"),
  theme = shinytheme("cerulean"), 
  # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen,
  # paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
  tabsetPanel(
    tabPanel("Data",icon = icon("database"),
          
             withTags(
               div(style="font:bold 14px 'Microsoft YaHei'; padding-top:10px",checked=NA,
                   p("Data structure") 
               )
             ),
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             hr(),
             withTags(
               div(style="font:bold 14px 'Microsoft YaHei'; text-align: center",checked=NA,
                      p("Distribution of missing values") 
                   )
               ),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",icon = icon("coins"),
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",icon = icon("server"),
             h4("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",icon = icon("cogs"),
             
             
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
           
             
             h5("Only 15 models are saved in the 'SavedModels' folder, while others without good performance or those would slow the startup are cleaned out."),
             
             hr(),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        hr(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe") 
               ),
               
            
               
######################################################### maintenance point ####################################################
tabPanel("1 LM Model",
         verbatimTextOutput(outputId = "lmModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "lmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","corr","lincomb")),
                  bsTooltip(id = "lmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "lmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "lmGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         
         tableOutput(outputId = "lmMetrics"),
         hr(),
         plotOutput(outputId = "lmModelPlot"),
         verbatimTextOutput(outputId = "lmRecipe"),
         verbatimTextOutput(outputId = "lmModelSummary2")
),

#---RLM  START--------
tabPanel("2 RLM Model",
         verbatimTextOutput(outputId = "rlmModelDiscription"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "rlmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("corr")),
                  bsTooltip(id = "rlmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "rlmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "rlmGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         
         tableOutput(outputId = "rlmMetrics"),
         hr(),
         verbatimTextOutput(outputId = "rlmModelSummary1"),
         plotOutput(outputId = "rlmModelPlot"),
         verbatimTextOutput(outputId = "rlmRecipe"),
         verbatimTextOutput(outputId = "rlmModelSummary2")
), 

 
tabPanel("3 LMstepAIC Model",
         verbatimTextOutput(outputId = "lmStepAICModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "lmStepAICPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "lmStepAICPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "lmStepAICGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "lmStepAICGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         
         tableOutput(outputId = "lmStepAICMetrics"),
         hr(),
         plotOutput(outputId = "lmStepAICModelPlot"),
         verbatimTextOutput(outputId = "lmStepAICRecipe"),
         verbatimTextOutput(outputId = "lmStepAICModelSummary2")
),

tabPanel("4 GLM Model",
         verbatimTextOutput(outputId = "glmModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "glmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","nzv","corr","center","scale","dummy")),
                  bsTooltip(id = "glmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "glmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "glmGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         
         tableOutput(outputId = "glmMetrics"),
         hr(),
         verbatimTextOutput(outputId = "glmModelSummary1"),
         verbatimTextOutput(outputId = "glmRecipe"),
         verbatimTextOutput(outputId = "glmModelSummary2")
),


tabPanel("5 BayesGLM Model",
         verbatimTextOutput(outputId = "bayesglmModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "bayesglmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","corr","center","scale","dummy")),
                  bsTooltip(id = "bayesglmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "bayesglmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "bayesglmGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         
         tableOutput(outputId = "bayesglmMetrics"),
         hr(),
         verbatimTextOutput(outputId = "bayesglmModelSummary1"),
         verbatimTextOutput(outputId = "bayesglmRecipe"),
         verbatimTextOutput(outputId = "bayesglmModelSummary2")
),



tabPanel("6 GLMnet Model",
         verbatimTextOutput(outputId = "GlmnetModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "GlmnetPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "GlmnetMetrics"),
         hr(),
         plotOutput(outputId = "GlmnetModelPlots"),
         verbatimTextOutput(outputId = "GlmnetRecipe"),
         verbatimTextOutput(outputId = "GlmnetModelSummary2")
),
tabPanel("7 PLS Model",
         verbatimTextOutput(outputId = "PlsModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "PlsPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h4("Resampled performance:"),
         tableOutput(outputId = "PlsMetrics"),
         hr(),
         plotOutput(outputId = "PlsModelPlots"),
         verbatimTextOutput(outputId = "PlsRecipe"),
         verbatimTextOutput(outputId = "PlsModelSummary2")
),



#---svmLinear2 -----------
    
tabPanel("8 SVMLinear2 Model",
         verbatimTextOutput(outputId = "SVMLinearSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "SVMLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "SVMLinearPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "SVMLinearGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "SVMLinearGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "SVMLinearMetrics"),
         hr(),
         plotOutput(outputId = "SVMLinearPlots"),
         verbatimTextOutput(outputId = "SVMLinearRecipe"),
         verbatimTextOutput(outputId = "SVMLinearSummary2")
), 


#---svmLinear2   end-----------



#---svmPoly -----------

tabPanel("9 SVMPoly Model",
         verbatimTextOutput(outputId = "svmPolySummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "svmPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "svmPolyPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "svmPolyGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "svmPolyGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "svmPolyMetrics"),
         hr(),
         plotOutput(outputId = "svmPolyPlots"),
         verbatimTextOutput(outputId = "svmPolyRecipe"),
         verbatimTextOutput(outputId = "svmPolySummary2")
), 


#---svmPoly   end-----------


#---'svmRadial' -----------

tabPanel("10 SVMRadial Model",
         verbatimTextOutput(outputId = "svmRadialSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "svmRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "svmRadialPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "svmRadialGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "svmRadialGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "svmRadialMetrics"),
         hr(),
         plotOutput(outputId = "svmRadialPlots"),
         verbatimTextOutput(outputId = "svmRadialRecipe"),
         verbatimTextOutput(outputId = "svmRadialSummary2")
), 


#---'svmRadial'   end-----------

 

#---'gaussprPoly' -----------

tabPanel("11 GaussprPoly Model(min RMSE)",
         verbatimTextOutput(outputId = "gaussprPolySummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "gaussprPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "gaussprPolyPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gaussprPolyMetrics"),
         hr(),
         plotOutput(outputId = "gaussprPolyPlots"),
         verbatimTextOutput(outputId = "gaussprPolyRecipe"),
         verbatimTextOutput(outputId = "gaussprPolySummary2")
), 


#---'gaussprPoly'   end-----------


#---'krlsPoly'   -----------
tabPanel("12 KrlsPoly Model",
         verbatimTextOutput(outputId = "krlsPolyModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "krlsPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE,  selected = c("knnimpute","dummy")),
                  bsTooltip(id = "krlsPolyPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "krlsPolyGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "krlsPolyGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "krlsPolyMetrics"),
         hr(),
         plotOutput(outputId = "krlsPolyModelPlots"),
         verbatimTextOutput(outputId = "krlsPolyRecipe") ,
         verbatimTextOutput(outputId = "krlsPolyModelSummary2") 
        
),
#---'krlsPoly'   end-----------


tabPanel("13 GAM Model",
         verbatimTextOutput(outputId = "gamModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "gamPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "gamPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "gamGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gamGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gamMetrics"),
         hr(),
         plotOutput(outputId = "gamModelPlots"),
         verbatimTextOutput(outputId = "gamRecipe"),
         verbatimTextOutput(outputId = "gamModelSummary2") 
         
), 



tabPanel("14 GAMBoost Model",
         verbatimTextOutput(outputId = "gamboostModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "gamboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "gamboostPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "gamboostGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gamboostGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gamboostMetrics"),
         hr(),
         plotOutput(outputId = "gamboostModelPlots"),
         verbatimTextOutput(outputId = "gamboostRecipe"),
         verbatimTextOutput(outputId = "gamboostModelSummary2") 
         
),  



tabPanel("15  Earth Model",
         verbatimTextOutput(outputId = "earthModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "earthPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "earthPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "earthGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "earthGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "earthMetrics"),
         hr(),
         plotOutput(outputId = "earthModelPlots"),
         verbatimTextOutput(outputId = "earthRecipe"),
         verbatimTextOutput(outputId = "earthModelSummary2") 
         
),  


tabPanel("16 GcvEarth Model",
         verbatimTextOutput(outputId = "gcvEarthModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "gcvEarthPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "gcvEarthPreprocess",
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1,
                  actionButton(inputId = "gcvEarthGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gcvEarthGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gcvEarthMetrics"),
         hr(),
         # plotOutput(outputId = "gcvEarthModelPlots"),
         verbatimTextOutput(outputId = "gcvEarthRecipe"),
         verbatimTextOutput(outputId = "gcvEarthModelSummary2")

),

tabPanel("17 Rpart Model",
         verbatimTextOutput(outputId = "RpartModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                  bsTooltip(id = "RpartPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "RpartMetrics"),
         hr(),
         plotOutput(outputId = "RpartModelPlots"),
         plotOutput(outputId = "RpartModelTree"),
         verbatimTextOutput(outputId = "RpartRecipe") 
),


tabPanel("18 Blackboost Model",
         verbatimTextOutput(outputId = "blackboostModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "blackboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "blackboostPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "blackboostGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "blackboostGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "blackboostMetrics"),
         hr(),
         plotOutput(outputId = "blackboostModelPlots"),
         verbatimTextOutput(outputId = "blackboostRecipe"),
         verbatimTextOutput(outputId = "blackboostSummary2")
),


tabPanel("19 RRF Model",
         verbatimTextOutput(outputId = "RRFModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "RRFPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "RRFPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "RRFGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "RRFGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "RRFMetrics"),
         hr(),
         plotOutput(outputId = "RRFModelPlots"),
         plotOutput(outputId = "RRFModelimprtance"),
         verbatimTextOutput(outputId = "RRFRecipe"),
         verbatimTextOutput(outputId = "RRFSummary2")
),



tabPanel("20 QRF Model",
         verbatimTextOutput(outputId = "qrfModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "qrfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "qrfPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "qrfGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "qrfGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "qrfMetrics"),
         hr(),
         plotOutput(outputId = "qrfModelPlots"),
         plotOutput(outputId = "qrfModelimprtance"),
         verbatimTextOutput(outputId = "qrfRecipe"),
         verbatimTextOutput(outputId = "qrfSummary2")
),




# tabPanel("16 XgbTree Model(abandoned)",
#          verbatimTextOutput(outputId = "xgbTreeModelSummary0"),
#          fluidRow(
#            column(width = 4, 
#                   selectizeInput(inputId = "xgbTreePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
#                   bsTooltip(id = "xgbTreePreprocess", 
#                             title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
#            ),
#            column(width = 1, 
#                   actionButton(inputId = "xgbTreeGo", label = "Train", icon = icon("play")),
#                   bsTooltip(id = "xgbTreeGo", title = "This will train or retrain your model")
#            )
#          ),
#          hr(),
#          h3("Resampled performance:"),
#          tableOutput(outputId = "xgbTreeMetrics"),
#          hr(),
#          plotOutput(outputId = "xgbTreeModelPlots"),
#          verbatimTextOutput(outputId = "xgbTreeRecipe"),
#          verbatimTextOutput(outputId = "xgbTreeSummary2")
# ),





tabPanel("21 KNN Model",
         verbatimTextOutput(outputId = "knnModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "knnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale")),
                  bsTooltip(id = "knnPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "knnGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "knnGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "knnMetrics"),
         hr(),
         plotOutput(outputId = "knnModelPlots"),
         verbatimTextOutput(outputId = "knnRecipe"),
         verbatimTextOutput(outputId = "knnModelSummary2") 
         
),  


tabPanel("22 Cubist Model",
         verbatimTextOutput(outputId = "cubistModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "cubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "cubistPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "cubistGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "cubistMetrics"),
         hr(),
         shinycssloaders::withSpinner(plotOutput(outputId = "cubistModelPlots")),
         shinycssloaders::withSpinner(plotOutput(outputId = "cubistModelPlot2")),
         
         verbatimTextOutput(outputId = "cubistModelSummary1"), 
         verbatimTextOutput(outputId = "cubistRecipe"),
         verbatimTextOutput(outputId = "cubistModelSummary2") 
         
), 


#---''neuralnet'' -----------

tabPanel("23 Neuralnet Model",
         verbatimTextOutput(outputId = "neuralnetSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "neuralnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE,  selected = c("knnimpute","dummy")),
                  bsTooltip(id = "neuralnetPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "neuralnetGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "neuralnetGo", title = "This will train or retrain your model")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "neuralnetMetrics"),
         hr(),
         plotOutput(outputId = "neuralnetPlots1"),
     
         verbatimTextOutput(outputId = "neuralnetRecipe"),
         verbatimTextOutput(outputId = "neuralnetSummary2")
) 

#---'neuralnet'   end-----------





           
             )
             ),
    tabPanel("Model Selection",icon = icon("filter"),
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""),selected=c("gaussprPoly"), inline = TRUE )
    ),
    tabPanel("Performance",icon = icon("gem"),
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
