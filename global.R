options(stringsAsFactors = F)
options(encoding = 'UTF-8')
set.seed(12345)
#load library
library(shiny)
library(shinydashboard)
library(sva)
library(fitdistrplus)
library(extraDistr)
library(umap)
library(ggplot2)
#import user defined functions
source('src/MyCombat.R')
environment(combat) <- asNamespace('sva')

effect_name="Please upload your sample information file"