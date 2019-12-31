options(stringsAsFactors = F)
options(encoding = 'UTF-8')
options(shiny.maxRequestSize=5*1024^3)
set.seed(12345)
#load library
library(shiny)
library(shinydashboard)
library(sva)
library(fitdistrplus)
library(extraDistr)
library(umap)
library(ggplot2)
library(plotly)
library(Biobase)
library(pvca)
library(randomForest)
library(preprocessCore)
library(openxlsx)

#library(bladderbatch)#testdata
#import user defined functions
source('src/MyCombat.R')
environment(my_it.sol) <- asNamespace('sva')
environment(combat) <- asNamespace('sva')
source('src/MyPVCA.R')
source('src/MyPriorDraw.R')
environment(drawPrior) <- asNamespace('sva')
source("src/MyRandomForest.R")
effect_name="Please upload your sample information file"
dataCheck<-function(d){
  error=NULL
  if(ncol(d)<2){
    error="Error: You may  set wrong separator!"
  }
  else if(sum(duplicated(d[,1]))>0){
    error="Error: The first column should be unique sample id/name!"
  }
  return(error)
}
missingValueReplace<-function(d,v){
  if(v!="none"){
    switch(v,
           "1" = d[is.na(d)]<-1,
           "0" = d[is.na(d)]<-0,
           minimum = d[is.na(d)]<-min(d,na.rm = T),
           "0.1" = d[is.na(d)]<-0.1*min(d,na.rm = T)
    )  
  }
  return(d)
}