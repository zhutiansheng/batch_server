source("global.R")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("BatchQC")


library(BatchQC)
#dataset 1
data(example_batchqc_data)
batch <- batch_indicator$V1
condition <- batch_indicator$V2

system.time(
  d1.combat<-ComBat(as.matrix(signature_data),batch,mod = NULL)
)
drawUMAP<-function(myd,label){
  myd[is.na(myd)]<-0
  myumap<-umap(myd,n_neighbors=10)
  mydf<-data.frame(myumap$layout)
  mydf$label<-label
  p<-ggplot(mydf,aes(x=X1, y=X2, colour=label)) + geom_point(size=3)+
    theme(  #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      #axis.text.x = element_text(vjust = 1,angle = 45),
      axis.line.x = element_line(color="black", size = 0.5),
      axis.line.y = element_line(color="black", size = 0.5),
      panel.background = element_blank())
  print(p)
}

#dataset 2
data(bladderdata)
pheno <- pData(bladderEset)
edata <- exprs(bladderEset)
batch <- pheno$batch  
condition <- pheno$cancer

#data set 3
data(protein_example_data)
