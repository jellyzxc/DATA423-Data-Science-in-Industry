#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Assignment2_XiaocuiZhang",
               theme = shinytheme("flatly"),#
               tabPanel(
                
                   "About the data sets",icon = icon("database"),
                    navlistPanel(
                         "Iris Data Set",
                            tabPanel("Information",
                             hr(),
                             withTags(
                                 
                                 div( style="font-size:18px ",checked=NA,
                                      h4("Basic information"),
                                      p("The data set contains 3 classes of 50 instances each, where each class refers to a type of iris plant. The attributes are shown below:"),
                                      ul(
                                          li("sepal length in cm"),
                                          li("sepal width in cm"),
                                          li("petal length in cm"),
                                          li("petal width in cm"),
                                          li("class:Setosa,Versicolour,Virginica")
                                      ),
                                      p("click",a(href="https://archive.ics.uci.edu/ml/datasets/iris","here"),"to get more information")
                                     )
                    
                             ),
                            ),
                            tabPanel("Structure",
                                     verbatimTextOutput(outputId = "StructureD1")
                            ),
                            tabPanel("Summary",
                                     verbatimTextOutput(outputId = "SummaryD1")  
                            ),
                            tabPanel("Data Table",
                                      DT::dataTableOutput(outputId = "RawTableD1") 
                            ),
 
                         
                            id = NULL, 
                            selected = NULL,
                            well = TRUE, 
                            fluid = TRUE,
                            widths = c(2, 10) 
                        ) 
               ), 
               tabPanel(
                    "Univariable ",icon = icon("anchor"),
                    
                     navlistPanel(
                     
                     tabPanel("DensityPlot",icon = icon("bell"),
                              
                              tags$p( "DensityPlot is used to look at the variable's distribution "), 
                        
                              selectizeInput(inputId = "Variables1", label = "Select variables:", choices = numericCols,multiple = FALSE, selected = numericCols[2]),
                              checkboxInput(inputId = "YeoJohnson", label = "Yeo Johnson transform ", value = FALSE),
                              # checkboxInput(inputId = "Fill", label = "filling the polygon with color ", value = TRUE),
                              plotlyOutput(outputId = "DensityPlot"),
                              helpText("Any anomoly or outlier detection can use data transforms as preprocessing steps.
                                        But we do not use the Yeo Johnson transform  
                                        before outlier detection as it does not contribute more to the detection in this case 
                                        due to the distribution of each variable.")
                              
                     ),
                     tabPanel("1 Boxplot" ,icon = icon("angle-double-right"),
                              tags$p( "It is easy to detect suspected outliers by boxplot, and we can add more variables to a boxplot, but here we only choose one as an example."),
                              selectizeInput(inputId = "Variables2", label = "Select variables:", choices = numericCols,multiple = FALSE, selected = numericCols[2]),
                              selectizeInput(inputId = "quartilemethod", label = "Quartilemethod:", choices = quartilemethod,multiple = FALSE, selected = quartilemethod[1]),
                              hr(),
                              fluidRow(
                                column(6,
                                       "Boxplot",
                                        plotlyOutput(outputId = "Boxplot")
                                ),
                                column(6,
                                       "Detected outliers",
                                        DT::dataTableOutput(outputId = "BoxplotTable") 
                                )
                              )
                              
                              
       
                     ),
                     tabPanel("2 OutlierPlot" ,icon = icon("angle-double-right"),
         
                              tags$p("'extremevalues' is a special package for univariate outlier detection, click",a(href="https://www.rdocumentation.org/packages/extremevalues/versions/2.3.2/topics/getOutliers","here"),"to get more information."),
                              
                              selectizeInput(inputId = "Variables3", label = "Select variables:", choices = numericCols,multiple = FALSE, selected = numericCols[2]),
                              selectizeInput(inputId = "distribution", label = "Select distribution:", choices = distribution,multiple = FALSE, selected = distribution[1]),
                              plotOutput(outputId = "outlier"),
                             
                              hr(),
                              fluidRow(
                                column(6,
                                       "Outliers detected by Method I",
                                       DT::dataTableOutput(outputId = "outlierTableK")
                                ),
                                column(6,
                                       "Outliers detected by Method II",
                                       DT::dataTableOutput(outputId = "outlierTableL") 
                                )
                              )
                     ),
                     tabPanel("3 UnivariateOutlierDetection" ,icon = icon("angle-double-right"), 
                              
                              tags$p("Univariate Outlier Detection (from OutlierDetection v0.1.1) takes a vector and finds its outliers using combination of different methods."),
                              
                              selectizeInput(inputId = "Variables4", label = "Select variables:", choices = numericCols,multiple = FALSE, selected = numericCols[2]),
                              selectizeInput(inputId = "method", label = "Select distance method:", choices = method,multiple = FALSE, selected = method[1]),
                             
                              hr(),
                              fluidRow(
                                column(6,
                                       "Plot",
                                       plotOutput(outputId = "OutlierDetection")
                                ),
                                column(6,
                                       "Detected outliers",
                                        DT::dataTableOutput(outputId = "OutlierDetectionTable") 
                                )
                              )
                              
                     ),
                     tabPanel( "Summary " ,icon = icon("calculator"), 
                               DT::dataTableOutput(outputId = "Summary1"),
                             
                       ),
                     
                     id = NULL, 
                     selected = NULL,
                     well = TRUE, 
                     fluid = TRUE,
                     widths = c(3,9) 
                     
                   )
                   
                   
                   
               ),
               tabPanel(
                   "Bivariable ",icon = icon("balance-scale"),
                                             
                   navlistPanel(
                     tabPanel("1 Color Plot" ,icon = icon("binoculars"),
                              tags$p("Colorplot plots the (two-dimensional) data using different symbols according to the robust mahalanobis distance based 
                                    on the mcd estimator with adjustment and using different colors according to the euclidean distances of the observations.
                                    Blue is typical for a little distance, whereas red is the opposite.
                                    The points laying outside of ellipsoids are treated outliers "),
                           selectizeInput(inputId = "Variables21", label = "Select variables:", 
                                          choices = numericCols,multiple = TRUE, selected = numericCols[1:2],
                                          options = list(maxItems = 2)) ,
                            helpText("maxItems=2"),
                           hr(),
                           fluidRow(
                             column(6,
                                    "Plot",
                                    plotOutput(outputId = "colorplot")
                             ),
                             column(6,
                                    "Detected outliers",
                                    DT::dataTableOutput(outputId = "colorplotTable") 
                             )
                           )
                          
                            
                     
                     ),
                     
                    
                     tabPanel("2 Distance-Distance Plot" ,icon = icon("binoculars"),
                              tags$p("DDplot plots the classical mahalanobis distance of the data against the robust mahalanobis distance based on the mcd estimator. 
                                       Different symbols and colours are used depending on the mahalanobis and euclidean distance of the observations"),
                              selectizeInput(inputId = "Variables22", label = "Select variables:", 
                                             choices = numericCols,multiple = TRUE, selected = numericCols[1:2],
                                             options = list(maxItems = 2)) ,
                              helpText("maxItems=2"),
                              
                              hr(),
                              fluidRow(
                                column(6,
                                       "Plot",
                                       plotOutput(outputId = "ddplot")
                                ),
                                column(6,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "ddplotTable") 
                                )
                               ) 
                              
        
                              
                     ),
                     tabPanel("3 Bag plot" ,icon = icon("binoculars") ,
                              
                              selectizeInput(inputId = "Variables23", label = "Select variables:", 
                                             choices = numericCols,multiple = TRUE, selected = numericCols[1:2],
                                             options = list(maxItems = 2)) ,
                              helpText("maxItems=2"),
                              
                              hr(),
                              fluidRow(
                                column(8,
                                       "Plot",
                                       plotOutput(outputId = "Bagplot")
                                ),
                                column(4,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "BagplotTable") 
                                )
                              ), 
                           
                     ),
                     tabPanel( "Summary " ,icon = icon("calculator"), 
                               DT::dataTableOutput(outputId = "Summary2")  
                     ),
                      
                     id = NULL, 
                     selected = NULL,
                     well = TRUE, 
                     fluid = TRUE,
                     widths = c(3, 9) 
                     
                   )                         
                   
               ),
               tabPanel(id="test",icon = icon("server"),  title="Multivariable",
                    # bsTooltip("test", title="Test Title", trigger = "hover"),
                    navlistPanel(
                     "Nominal variables",
                     tabPanel("Mosaic" ,icon = icon("caret-right"),
                              hr(),
                              helpText(" We use Titanic Data Set to plot the graph"),
                              selectizeInput(inputId = "Variables31", label = "Select variables:", 
                                             choices = titanicCols,multiple = TRUE, selected = titanicCols),
                              plotOutput(outputId = "Mosaic"), 
                              
                              
                     ),
                     
                     "Numeric variables",
                     
                     tabPanel("Instruction" ,icon = icon("info-circle"),
                              
                              hr(),
                              withTags(
                                
                                div( checked=NA,
                                     h4("Explanatory note"),
                                     p("In the bivariate section, methods can be applied for two dimensional matrix or data frame only."),
                                     p("In this section,method 1-7 are outlier detection methods from OutlierDetection package, 
                                        and they can be used in multivariate anomaly detection(including bivariate data)."),  
                                     p("For bivariate data, the plot changes to a scatterplot.
                                         But here we just give a demonstration and show the 3Dplot without considering the bivariate situation.")
                                )
                                
                              ) 
                              
                     ),
                     tabPanel("1 Mahalanobis Distance-maha",icon = icon("caret-right"),
                              tags$p("Maha computes Mahalanibis distance an observation and based on the Chi square cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported based on its p values."),
                              selectizeInput(inputId = "Variables32", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              sliderInput(inputId = "cutoff1", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                              helpText("cutoff is the percentile threshold used for distance. As the cutoff increases, the number of outliers decreases."),
                              hr(),
                              fluidRow(
                                column(5,
                                       "Mahaplot",
                                        plotlyOutput(outputId = "mahaplot")
                                ),
                                column(7,
                                       "Detected outliers",
                                        DT::dataTableOutput(outputId = "mahaTable") 
                                )
                              )
                     ),
                     tabPanel("2 Kth NN Distance-nnk" ,icon = icon("caret-right"),
                              
                              tags$p("nnk computes kth nearest neighbour distance of an observation and based on the bootstrapped cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported and it is the bootstrap estimate of probability of the observation being an outlier"),
                              
                              selectizeInput(inputId = "Variables33", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              selectizeInput(inputId = "method1", label = "Select distance method:", choices = method,multiple = FALSE, selected = method[1]),
                              sliderInput(inputId = "cutoff2", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                             

                               hr(),
                               fluidRow(
                                column(5,
                                       "NNkplot",
                                       plotlyOutput(outputId = "nnkplot")
                                ),
                                column(7,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "nnkTable") 
                                )
                              )
                     ),
          
                     tabPanel("3 PCOutlierDetection" ,icon = icon("caret-right"),
                              tags$p("OutlierDetection finds outlier observations for the principal component space using different methods and based on all the methods considered, labels an observation as outlier(intersection of all the methods). For bivariate data, it also shows the scatterplot of the data with labelled outliers."),
                              selectizeInput(inputId = "Variables35", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              selectizeInput(inputId = "method5", label = "Select distance  method:", choices = method,multiple = FALSE, selected = method[1]),
                              sliderInput(inputId = "cutoff5", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                            
                              hr(),
                              fluidRow(
                                column(5,
                                       "PCOutlierDetection plot",
                                        plotlyOutput(outputId = "pcplot"),
                                        helpText("The plot is based on principal components")
                                ),
                                column(7,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "pcTable") 
                                )
                              )
                     ),
                     tabPanel("4 Dispersion Based-disp " ,icon = icon("caret-right"),
                              
                              tags$p("Disp computes LOO dispersion matrix for each observation(dispersion matrix without cosidering the current observation) and based on the bootstrapped cutoff for score(difference between determinant of LOO dispersion matrix and det of actual dispersion matrix), labels an observation as outlier"),
                              selectizeInput(inputId = "Variables36", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              sliderInput(inputId = "cutoff6", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                              
                              hr(),
                              fluidRow(
                                column(5,
                                       "Dispplot",
                                        plotlyOutput(outputId = "dispplot"),
                                       
                                ),
                                column(7,
                                       "Detected outliers",
                                        DT::dataTableOutput(outputId = "dispTable") 
                                )
                              )
                     ),
                     tabPanel("5 RKOF-dens" ,icon = icon("caret-right"),
                              tags$p("Dens computes outlier score of an observation using DDoutlier package(based on RKOF algorithm) and based on the bootstrapped cutoff, labels an observation as outlier."),
                              selectizeInput(inputId = "Variables37", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              sliderInput(inputId = "cutoff7", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                              sliderInput(inputId = "c", label = "C", min = 0, max = 5, step = 1, value = 1),
                              helpText("C:Multiplication parameter for k-distance of neighboring observations"),
                              hr(),
                              fluidRow(
                                column(5,
                                       "Densplot",
                                        plotlyOutput(outputId = "densplot") 
                                       
                                ),
                                column(7,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "densTable") 
                                )
                              )

                     ),
                     tabPanel("6 OutlierDetection" ,icon = icon("caret-right"),
                              tags$p("OutlierDetection finds outlier observations for the data using different methods and based on all the methods considered, labels an observation as outlier."),
                              selectizeInput(inputId = "Variables38", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              selectizeInput(inputId = "method8", label = "Select distance method:", choices = method,multiple = FALSE, selected = method[1]),
                              sliderInput(inputId = "cutoff8", label = "cutoff", min = 0.60, max = 1.00, step = 0.01, value = 0.95),
                              
                              hr(),
                              fluidRow(
                                column(5,
                                       "Plot",
                                       plotlyOutput(outputId = "outplot") 
                                       
                                ),
                                column(7,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "outTable") 
                                )
                              )
                     ),
                     tabPanel("7 Depth Based-depthout" ,icon = icon("caret-right"),
                              
                              tags$p("Depthout computes depth of an observation using depthTools package and based on the bootstrapped cutoff, label an observation as outlier."),
                              
                              selectizeInput(inputId = "Variables34", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              sliderInput(inputId = "cutoff3", label = "cutoff", min = 0.00, max = 0.1, step = 0.01, value = 0.05),
                              helpText("cutoff is the percentile threshold used for depth.As the cutoff decreases, the number of outliers increases."),
                              
                              hr(),
                              fluidRow(
                                column(5,
                                       "Depthoutplot",
                                        plotlyOutput(outputId = "depthoutplot")
                                ),
                                column(7,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "depthoutTable") 
                                )
                              )
                              
                     ),
                     tabPanel("8 OutliersO3" ,icon = icon("caret-right"),
                              
                              selectizeInput(inputId = "Variables39", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              selectizeInput(inputId = "method9", label = "Select method:", choices = methodO3,multiple = FALSE, selected = methodO3[1]),
   
                              hr(),
                              fluidRow(
                                column(6,
                                       "Plot",
                                       plotOutput(outputId = "O3plot") 
                                ),
                                column(6,
                                       "Detected outliers",
                                       DT::dataTableOutput(outputId = "O3Table") 
                                )
                              )  
                              
                     ),
                     
                     tabPanel( "Summary " ,icon = icon("calculator"), 
                               helpText("As there are many variables, we show the indexes of the observations only. You could filter the index to find out which method treated the corresponding observation as an outlier. "),
                               DT::dataTableOutput(outputId = "Summary3",width = "600px") 
                     ),
                     
                     id = NULL, 
                     selected = NULL,
                     well = TRUE, 
                     fluid = TRUE,
                     widths = c(3, 9) 
                     
                   )
                   
               ),
               tabPanel(

                  "Model based ",icon = icon("globe"),
                  
                   navlistPanel(
                      
                     "Instruction",
                     tabPanel("Instruction",icon = icon("info-circle"),
                              hr(),
                              withTags(
                                
                                div(  
                                     h4("Personal opinion"),
                                     p(" When we talk about model-based outlier detection, the goal is to look for outliers in the model's residuals rather than in the outcome and predictors."),
                                     p(" For example, Cook's distance is a measure of how much a linear regression is affected by each observation.The linear regression model is the ultimate goal,and detecting the outliers serves a better linear regression model."),  
                                     p(" In this context I would treate other methods like DBScan,LOF,One-classification SVM and Isolation Forest as functions or models for multivariate anomaly detection.")
                                )
                              )
                        ),
                     
                      "Model based",
                      tabPanel("Cook\'s  Distance" , icon = icon("atom"),
                           
                        
                           withTags(
                             
                             div( style="font-size:15px ",checked=NA,
                                  p("Cook\'s distance is a measure of how much a linear or generalized linear model is affected by each observation.We may consider an observation unduly influential if it has a Cook\'s distance value of more than 0.4.But we can set the threshold circumstantially."),
                                  p("In this example, we assume that the Sepal Length has a linear relationship with other variables and create the model as below:"),  
                                  p("glmfit = glm(Sepal.Length ~Sepal.Width+Petal.Length+Petal.Width+Species, family = gaussian, data = iris)")
                             )  
                           ),
                           numericInput(inputId='threshold', label = 'Threshold adjuster(multiply by mean distance)', min = 1, max = 10,value = 4,width = "400px"),
                           hr(),
                           fluidRow(
                             column(5,
                                    "Cook's distance",
                                    plotlyOutput(outputId = "cook"),
                             ),
                             column(7,
                                    "Unduly influential observations",
                                     DT::dataTableOutput(outputId = "cooktable") 
                             )
                           )
                           
                               
                      ),
                     
                     "Others",
                     tabPanel("9 DBScan" , icon = icon("caret-right"),
                              
                         selectizeInput(inputId = "Variables42", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols,width = "400px"),
                         numericInput(inputId='K', label = 'K: number of nearest neighbors used (use minPoints)', min = 0, max = 100,value = 4,width = "400px"),
                         
                         hr(),
                         fluidRow(
                           column(5,
                                  "The K-Nearest Neighbor Distance",
                                   plotOutput(outputId = "knndist"),
                           ),
                           column(7,
                                  "Detected outliers",
                                   numericInput(inputId='eps', label = 'eps', min = 0, max = 1,step=0.1,value = 0.6),
                                  
                                   verbatimTextOutput(outputId = "noisepointsSum"),
                                   DT::dataTableOutput(outputId = "NoiseTable") 
                           )
                         )  
                              
                              
                     ),
                     tabPanel("10 LOF" , icon = icon("caret-right"),
                              
                              selectizeInput(inputId = "Variables43", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              numericInput(inputId='K3', label = 'K: size of the neighborhood', min = 0, max = 100,value = 4),
                              helpText("While a small k has a more local focus and it is more erroneous when having much noise in the data. A large k, however, can miss local outliers."),
                              hr(),
                              fluidRow(
                                
                                column(5,
                                       "LOF plot",
                                       # plotOutput(outputId = "lof"),
                                       plotlyOutput(outputId = "lof3D")
                                        # conditionalPanel("lof1", plotOutput(outputId = "lof")),
                                        # conditionalPanel("lof1", plotlyOutput(outputId = "lof3D"))
                                ),
                                column(7,
                                       "LOF values",
                                        helpText("when LOF is larger than 1, outlies may exist.Cells are in orange if LOF score>2 and in pink when LOF score>1"),
                                        DT::dataTableOutput(outputId = "lofscore") 
                                )
                              )   
                              
                     ),
                     tabPanel("11 One-classification SVM" , icon = icon("caret-right"),
                              helpText("one-classification SVM is for novelty detection"),
                              selectizeInput(inputId = "Variables44", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols),
                              selectizeInput(inputId = "kernal", label = "Set kernal:",  choices = kernal,multiple = F, selected = kernal[1]),
                              
                               hr(),
                               
                               DT::dataTableOutput(outputId = "svm") 
                               
                     ),
                     
                     tabPanel("12 Isolation Forest" , icon = icon("caret-right"),
 
                              selectizeInput(inputId = "Variables45", label = "Select variables:",  choices = AllCols,multiple = TRUE, selected = AllCols,width="400px"),
                                
                              numericInput(inputId='ntree', label = 'ntree:Number of binary trees to build for the model', min = 5, max = 100,value =10,width="400px"),
                                
                              numericInput(inputId='ndim', label = 'ndim:Number of columns to combine to produce a split', min = 1, max = 4,value =1,width="400px"),
                               
 
                              
                              hr(),
                              fluidRow(

                                column(5,
                                       "Plot",
                                        plotlyOutput(outputId = "forestPlot")
                                ),
                                column(7,
                                       "Score",
                                        helpText("The standardized outlier score, where values closer to 1 indicate more outlierness, while values closer to 0.5 indicate average outlierness, and close to 0 more averageness (harder to isolate)"),
                                        DT::dataTableOutput(outputId = "forestTable")
                                )
                              ) 
                     ),
                      id = NULL, 
                      selected = NULL,
                      well = TRUE, 
                      fluid = TRUE,
                      widths = c(3, 9) 
                      
                  )
                   
               ),
               tabPanel(
                   "A complex example",icon = icon("user-secret"),
                   
                   navlistPanel(
                      
                     tabPanel("Faultiness",
                              hr(),
                              withTags(
                                
                                div( style="font-size:16px ",checked=NA,
                                     h4("What is the problem ?"),
                                     p("When we applied methods like one-classification SVM or Isolation Forest to detect the potential outliers, we trained the model using the whole data set. Then we predicted the outliers in the same data set based upon the model trained by SVM or Isolation Forest. That a learned process, and what we did will cause data leakage.
                                      Therefore, we need to use cross-validation (or boot strapping) to find out the outliers without bias."),
                                )
                                
                              ),
                     ),
                     tabPanel("Solution",
                              
                              selectizeInput(inputId = "Variables51", label = "Select variables:",  choices = numericCols,multiple = TRUE, selected = numericCols,width="400px"),
                               selectizeInput(inputId = "kernal5", label = "Set kernal:",  choices = kernal,multiple = F, selected = kernal[1],width="400px"),
                               numericInput(inputId='KFold', label = 'K:Number of folds', min = 2, max = 20,value =10,width="400px"),
                              
                               hr(),
                               fluidRow(
                                
                                column(5,
                                       tags$h4("One-classification SVM applying k-Fold Cross Validation"), 
                                        
                                       DT::dataTableOutput(outputId = "kftable"),
                                ),
                                column(1),
                                column(5,
                                       tags$h4("One-classification SVM"), 
                                       
                                       DT::dataTableOutput(outputId = "svm2") 
                                )
                              ),
                              hr(),
                              withTags(
                                
                                div( style="font-size:16px ",checked=NA,
                                     p("More potential outliers have been detected when applying k-Fold Cross Validation"),
                                )
                                
                              )
  
                              
                     ), 
                     id = NULL, 
                     selected = NULL,
                     well = TRUE, 
                     fluid = TRUE,
                     widths = c(2, 10) 
                   ) 
               ),
               useShinyjs()
  ) 
    
)