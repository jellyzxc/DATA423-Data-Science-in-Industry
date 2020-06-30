#author:XiaoxuiZhang 16142030
#423Assignment1  2020-03-04

#prepare values for selector  
choices_cat <- colnames(Qualitative_data%>%select(-ID))  #remove ID 
choices_num <- colnames(Quantitative_data)  
choices_all <- colnames(newdata)
domChoices <- c("l","f","r","t","i","p")

## UI
shinyUI(
    navbarPage("Assignment1_XiaocuiZhang",
               theme = shinytheme("cerulean"),#flatly
               tabPanel("Summary",
                        
                        navlistPanel(
                            "All variables",
                            tabPanel("structure & summary",
                                     
                                     tabsetPanel(
                                         
                                         tabPanel("structure",
                                                  verbatimTextOutput(outputId = "SummaryR1")
                                         ),
                                         tabPanel("summary",
                                                  verbatimTextOutput(outputId = "SummaryR2")  
                                         )
                                     )
                            ),
                           
                            "Nominal",
                            tabPanel("All factors",
                                     tags$h4("The summary of all categorical variables"),
                                     verbatimTextOutput(outputId = "SummaryCate")
                                     ),
                            tabPanel("Ordinal factors",
                                     tags$h4("The summary of ordinal factors"),
                                     verbatimTextOutput(outputId = "SummaryOrd")
                                     ),
                            "Numeric",
                            tabPanel("Numeric variables",
                                     tags$h4("The summary of numeric variables"),
                                     verbatimTextOutput(outputId = "SummaryCont")
                                     ),
                              "Date",
                              tabPanel("Date",
                                       tags$h4("The summary of Dates"),
                                       verbatimTextOutput(outputId = "SummaryDate")
                            ),
                            id = NULL, 
                            selected = NULL,
                            well = TRUE, 
                            fluid = TRUE,
                            widths = c(2, 10) 
                        ) 
                        ),  

               tabPanel("Visualisation",
                        navlistPanel(

                          "Missing values",
                          
                          tabPanel("Vis_miss",
                                   selectizeInput(inputId = "VariablesC", label = "Select variables:", choices = choices_all,
                                                  multiple = TRUE, selected = choices_all),
                                   checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE),
                                   plotOutput(outputId = "Missing")
                                   ),
                            "Novelties",
                             tabPanel("Mosaic",
                                   selectizeInput(inputId = "VariablesA", label = "Show variables:", choices = choices_cat,
                                                  multiple = TRUE, selected = choices_cat[2:5]),
                                   helpText("The NAs were removed here"),
                                   plotOutput(outputId = "Mosaic")
                                   ),
                              tabPanel("Boxplot",
                                     selectizeInput(inputId = "VariablesB", label = "Select variables:", choices = choices_num,
                                                    multiple = TRUE, selected = choices_num[2:15]),
                                     
                                     checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                                     checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                                     sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 4, step = 0.1, value = 1.5),
                                     plotOutput(outputId = "Boxplot")
                               ),
                          
                            "Correlation",
                            tabPanel("Corrgram",
                                     selectizeInput(inputId = "VariablesD", label = "Select variables:", choices = choices_num,
                                                    multiple = TRUE, selected = choices_num),
                                     hr(),
                                     
                                     plotOutput(outputId = "Corrgram"),
                                     checkboxInput(inputId = "abs", label = "Grouping uses absolute correlation", value = FALSE) 
                                     
                                     #Use absolute value of correlations for clustering? Default FALSE
                                     
                            ),
                            
                            tabPanel("Corrgram-fixed",

                                     fluidRow(
                                         
                                         column(6,
                                                plotOutput(outputId = "Corrgram1"),
                                                plotOutput(outputId = "Corrgram3")
               
                                         ),
                                         column(6,
                                                
                                                plotOutput(outputId = "Corrgram2"),
                                                plotOutput(outputId = "Corrgram4")  
                         
                                         )
                                     )
                                     
                                      
                            ),
                            
                            tabPanel("Pairs-fixed",
                                      
                                     fluidRow(
                                       
                                       column(6,
                                              plotOutput(outputId = "Pairs1"),
                                              plotOutput(outputId = "Pairs3")
                                              
                                       ),
                                       column(6,
                                              
                                              plotOutput(outputId = "Pairs2"),
                                              plotOutput(outputId = "Pairs4")  
                                              
                                       )
                                     ) 

                            ),
                            
                            tabPanel("Mixed Pairs",
                                     
                                     selectizeInput(inputId = "VariablesE", label = "Select a nominal variable:", choices = choices_cat,
                                                    multiple = FALSE, selected = choices_cat[3] ),
                                     selectizeInput(inputId = "VariablesF", label = "Select some numeric variables:", choices = choices_num,
                                                    multiple = TRUE, selected = choices_num[c(2,3,6)]),
                                     helpText("The NAs were removed here"),
                                     plotOutput(outputId = "MixedPairs") 
 
                            ),
                            
                            "Continuity",
                            tabPanel("Rising-value chart",
                                     selectizeInput(inputId = "VariablesI", label = "Select variables:", choices = choices_num,
                                                    multiple = TRUE, selected = choices_num[c(2,3,8)]),
                                     checkboxInput(inputId = "scale", label = "scaled", value = TRUE),
                                     hr(),
                                    plotOutput(outputId = "Continuity")
                                      
                            ),
                            "Homogeneity",
                            tabPanel("Homogeneity",
                                     selectizeInput(inputId = "VariablesJ", label = "Select variables:", choices = choices_num,
                                                    multiple = TRUE, selected = choices_num[2:8]),
                                     hr(),
                                     plotOutput(outputId = "Homogeneity")
                                     
                            ),
                          
                          "Time Series",
                          tabPanel("time series of numeric data ",
                                   selectizeInput(inputId = "VariablesT", label = "Select variables:", choices = choices_num,
                                                  multiple = TRUE, selected = choices_num[1]),
                                   hr(),
                                   plotOutput(outputId = "TimeSeries")
                                   
                          ),
                            id = NULL, 
                            selected = NULL,
                            well = TRUE, 
                            fluid = TRUE,
                            widths = c(2, 10) 
                        )   
                      
                        ),
                   tabPanel("Data table",
                            fluidRow(
                              helpText(""),
                              column(1,
                                     selectizeInput(inputId = "VariablesH", label = "Select variables", choices = choices_all,
                                                    multiple = TRUE, selected = choices_all[1:12]),
                                     helpText(""),hr(), helpText(""),
                                     checkboxInput("order", "Column ordering", value=T),
                                     selectInput("selection", "Row selection type", choices=c("none","single","multiple"), selected = "none"),
                                     selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                     selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
                              ),
                              column(11,
                                     helpText(""),     
                                     DT::dataTableOutput(outputId = "rawdata")
                              )
                            )
                                              
                        ),
               
               useShinyjs()
    )
    
 
)   
    
    
   



