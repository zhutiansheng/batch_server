library(fitdistrplus)
library(extraDistr)
library(umap)
library(ggplot2)
library(Biobase)
library(pvca)
library(BiocParallel)
source("src/MyPVCA.R")
source("test/MyCombat2.R")
source("src/MyPriorDraw.R")
source("test/functionsFromsva2.R")
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

library(BatchQC)

analysisBatch<-function(dat,batch,type,sample_info,out_pdf){
  pdf(out_pdf)
  pvcaobj<-pvcaBF(t(dat),sample_info,c("batch","type"),0.1)
  pieDraw(pvcaobj)
  drawUMAP(t(dat),as.factor(type))
  drawUMAP(t(dat),as.factor(batch))
  
  modcombat = model.matrix(~as.factor(type), data=sample_info)
  
  for (param in c("auto","parameter","noparameter")) {
    t<-system.time(dat.combat<-combat(as.matrix(dat),batch,mod = modcombat,par.prior=param))
    message(param)
    print(t)
    dat2<-dat.combat$bayesdata
    dat2[is.na(dat2)]<-0
    combat_pvcaobj<-pvcaBF(t(dat2),sample_info,c("batch","type"),0.1)
    pieDraw(combat_pvcaobj)
    drawUMAP(t(dat2),as.factor(type))
    drawUMAP(t(dat2),as.factor(batch))
    if(param == "auto"){
      print(dat.combat$additiondata$passTest)
      for(b in unique(batch))
        drawPrior(dat.combat$additiondata,b)
    }

  }
  dev.off()
}
#########################################################
#dataset 1
data(example_batchqc_data)
batch <- batch_indicator$V1
condition <- batch_indicator$V2
sample_info<-data.frame(batch,type=condition)
rownames(sample_info)<-colnames(signature_data)
analysisBatch(signature_data,batch,condition,sample_info,"signature.pdf")

#####
#dataset 2
#####
library(bladderbatch)
data(bladderdata)
sample_info <- pData(bladderEset)
edata <- exprs(bladderEset)
batch <- sample_info$batch  
type <- as.vector(sample_info$cancer)
colnames(sample_info)[4]<-"type"
analysisBatch(edata,batch,type,sample_info,"bladder.pdf")

#data set 3
data(protein_example_data)
dat<-protein_data
sample_info<-protein_sample_info[,c("samplename", "Batch","category")]
sample_info$samplename<-paste0("X",sample_info$samplename)
rownames(sample_info)<-sample_info[,1]
colnames(sample_info)<-c("samplename", "batch","type")
sample_info<-sample_info[colnames(dat),]
type<-as.vector(sample_info$type)
batch<-sample_info$batch
analysisBatch(dat,batch,type,sample_info,"protein.pdf")
