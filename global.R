options(stringsAsFactors = F)
options(encoding = 'UTF-8')
options(shiny.maxRequestSize=500*1024^2)
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
#library(bladderbatch)#testdata
#import user defined functions
source('src/MyCombat.R')
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