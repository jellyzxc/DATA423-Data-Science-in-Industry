#author:XiaoxuiZhang 16142030
#423Assignment2  2020-03-20

library(shiny)
library(readr)
library(dplyr)
library(datasets)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer) 
library(DT)
library(vcd)
library(recipes)

library(mvoutlier)
library(extremevalues)
library(outliers)
library(OutliersO3)
library(OutlierDetection )

library(bestNormalize) 
library(dbscan)
library(e1071)
library(isotree)
library(caret)

library(plotly)
 
#ui  
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shiny)


library(aplpack, warn.conflicts = FALSE)   #bagplot
 


library(titanic)
data(iris)
titanic=titanic::titanic_train


titanic=titanic::titanic_train
titanic <- within(titanic, {
  Survived[Survived==0] <- 'no'
  Survived[Survived==1] <- 'yes'
})

titanic$Pclass=as.factor(titanic$Pclass)
titanic$Sex=as.factor(titanic$Sex)
titanic$Survived=as.factor(titanic$Survived)

titanicCols <-c("Survived","Pclass","Sex")

AllCols <- colnames(iris) 
 
numericCols <- colnames(iris)[1:4]
numericData<-iris[,numericCols, drop = FALSE]


quartilemethod<-c("linear","inclusive","exclusive")
 

distribution<-c("normal","lognormal", "exponential", "pareto", "weibull")

method<-c("euclidean", "maximum", "manhattan", "canberra", "binary" , "minkowski")

methodO3<-c("PCS","HDo","FastPCS", "BAC" ,"adjOut", "DDC","MCD")

kernal<-c("radial","linear", "polynomial" , "sigmoid")


#for comparsion

menthod_1=c("Boxplot","extremevalues-I","extremevalues-II","UnivariateOT")
menthod_2=c("Color Plot","Distance-Distance Plot","Bag plot")
menthod_3=c("Mahalanobis Distance-maha","Kth NN Distance-nnk","PCOutlierDetection","Dispersion Based-disp",
            "Robust Kernal-Based Outlier Factor-dens","OutlierDetection","Depth Based-depthout","OutliersO3")


 
#reactive  global Values for summary

r11  <- reactiveValues(data=NULL)
r12_1  <- reactiveValues(data=NULL)
r12_2  <- reactiveValues(data=NULL)
r13 <- reactiveValues(data=NULL)

 
 
r21  <- reactiveValues(data=NULL)
r22  <- reactiveValues(data=NULL)
r23 <- reactiveValues(data=NULL)


r31  <- reactiveValues(data=NULL)
r32  <- reactiveValues(data=NULL)
r33 <- reactiveValues(data=NULL)
r34  <- reactiveValues(data=NULL)
r35  <- reactiveValues(data=NULL)
r36 <- reactiveValues(data=NULL)
r37 <- reactiveValues(data=NULL)
r38 <- reactiveValues(data=NULL)


