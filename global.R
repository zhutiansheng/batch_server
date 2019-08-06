options(stringsAsFactors = F)
options(encoding = 'UTF-8')
set.seed(12345)
#load library
library(shiny)
library(shinydashboard)
library(sva)
library(fitdistrplus)
library(extraDistr)

#import user defined functions
source(MyCombat.R)
environment(combat) <- asNamespace('sva')