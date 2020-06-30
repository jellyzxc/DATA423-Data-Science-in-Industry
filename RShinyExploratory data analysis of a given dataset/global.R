#author:XiaoxuiZhang 16142030
#423Assignment1  2020-03-04

library(shiny)
library(readr)
library(dplyr)
library(datasets)
library(tidytext)
library(tidyverse)
library(tibble)
library(reshape2)
library(ggplot2)
library(lubridate)
library(RColorBrewer) 
library(corrgram)
library(visdat)
library(vcd)
library(GGally)
library(DT)
library(graphics)

#ui  
library(shinythemes)
library(shinyjs)



###==========load raw data
rawdata <- read.csv("Ass1Data.csv",header = TRUE)

# alter "Date" from factor to real date,
newdata<-rawdata%>%mutate(Date=as.Date(ymd(Date)))  #%>%mutate(Year= year(Date),Week= as.factor(week(Date)))
# nominal variables with apparent order are Ordinal variables 
newdata$Priority <- factor(newdata$Priority , order = TRUE, levels = c("Low", "Medium", "High"))
newdata$Price <- factor(newdata$Price , order = TRUE, levels = c("Cheap", "Costly", "Extravagant"))
newdata$Speed <- factor(newdata$Speed , order = TRUE, levels = c("Slow", "Medium", "Fast"))
newdata$Duration <- factor(newdata$Duration , order = TRUE, levels = c("Short", "Long", "Very Long"))
newdata$Temp <- factor(newdata$Temp , order = TRUE, levels = c("Cold", "Warm", "Hot"))

# #as.logical   --should be logic
# levels(newdata$Agreed) <- c(FALSE,TRUE)
# newdata$Agreed <- as.logical(newdata$Agreed)

####========== prepare data sets for visualisation  

# nominal  data
Qualitative_data<-newdata%>%select(2:3,5:14)
Ordinal_data<-newdata%>%select(5:9)
# numeric data
Quantitative_data<-newdata%>%select(1,15:44)

# date
Date_data<- newdata%>%select(4)

# group numeric data
qq<-Quantitative_data[-1]
q1<-qq[c(1,2,5,6,7,8,9,10)]
q2<-qq[c(3,4,13,17,22,24,27)]
q3<-qq[c(11,12,14,15,16,18,19,20)]
q4<-qq[c(21,23,25,26,28,29,30)]





