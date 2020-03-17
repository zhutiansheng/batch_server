# Introduction
Batch effects are unwanted sources of data variation that obscure biological signals. How to eliminate batch effects effectively is one of the biggest challenges in omics data analysis. BatchServer is an open-source R-based web server that enables interactive evaluation and automatic correction of batch effects for a variety of large-scale omics data sets. 

# Installation
Install all the packages in global.R.
## Install the devtools
install.packages("devtools")
library(devtools)
## Install packages on CRAN
install.packages(c("shiny","shinydashboard",”fitdistrplus”, "extraDistr","umap","ggplot2","plotly","openxlsx”)
## Packages on Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite(c("sva","Biobase","pvca","preprocessCore")) 

# Running
Set the working directory as the path of app.R and run the app.R in R enviroment. 
#  Reporting problems
 If you have any problem, please report it at gitHub issue tracker https://github.com/zhutiansheng/batch_server/issues
