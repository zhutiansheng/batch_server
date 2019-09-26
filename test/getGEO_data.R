##Batch effect removalmethods for microarray gene expression data integration: a survey
library(GEOquery)
library(ggplot2)
library(sva)
source("../src/MyCombat.R")
environment(combat) <- asNamespace('sva')
library(fitdistrplus)
library(extraDistr)
library(preprocessCore)
source("../src/MyPriorDraw.R")
environment(drawPrior) <- asNamespace('sva')
source("../src/MyPVCA.R")
library(pvca)
library(umap)
pieDraw<-function(pvcaobj){
  dat<-as.vector(unlist(pvcaobj$dat))
  Source<-pvcaobj$label
  df<-data.frame(dat,Source)
  #df$dat<-as.numeric(df$dat)
  df$percent<-round(df$dat/sum(df$dat)*100,3)
  df$Source<-paste0(Source," (",df$percent,"%)")
  p<-ggplot(df, aes(x = "", weight =percent , fill = Source)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") + xlab('') + ylab('') +
    #geom_text(aes(x = 1.3, y = Data, label = Sample))+
    theme_minimal()  
  p
}
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
##get data and process
gse19804download<-getGEO("GSE19804")
gse19804<-gse19804download$GSE19804_series_matrix.txt.gz
gse19804eset<-exprs(gse19804)
gse19804pheno<-pData(phenoData(gse19804))

gse10072downlaod<-getGEO("GSE10072")
gse10072<-gse10072downlaod$GSE10072_series_matrix.txt.gz
gse10072eset<-exprs(gse10072)
gse10072pheno<-pData(phenoData(gse10072))

# table(gse10072pheno$characteristics_ch1)
# descEset<-function(eset){
#   for(name in colnames(eset)){
#     if(length(unique(as.vector(eset[,name])))<20){
#            print(name)
#       print(table(eset[,name])) 
#     }
# 
#   }
# }
# descEset(gse10072pheno)
# descEset(gse19804pheno)

batch<-rep(c(1,2),c(ncol(gse19804eset),ncol(gse10072eset)))
comm_probe<-intersect(rownames(gse19804eset),rownames(gse10072eset))
gse_eset<-cbind(gse19804eset[comm_probe,],gse10072eset[comm_probe,])

type10072<-as.vector(gse10072pheno[colnames(gse10072eset),"source_name_ch1" ])
type10072<-gsub("Adenocarcinoma of the Lung","Cancer",type10072,fixed = T)
type10072<-gsub("Normal Lung Tissue","Normal",type10072,fixed = T)

type19804<-as.vector(gse19804pheno[colnames(gse19804eset),"characteristics_ch1"])
type19804<-gsub("tissue: lung cancer","Cancer",type19804,fixed = T)
type19804<-gsub("tissue: paired normal adjacent","Normal",type19804,fixed = T)
type<-c(type19804,type10072)
sample<-c(colnames(gse19804eset),colnames(gse10072eset))
sample_info<-data.frame(sample,batch,type)
rownames(sample_info)<-sample_info[,1]
sample_info$type<-gsub("cancer","Cancer",sample_info$type)
type<-sample_info$type  

####quantile normalization
gse_eset_qn<-normalize.quantiles(gse_eset)
colnames(gse_eset_qn)<-colnames(gse_eset)

###
# pvcaobj<-pvcaBF(t(gse_eset),sample_info,c("batch","type"),0.1)
# pieDraw(pvcaobj)

pvcaobj_qn<-pvcaBF(t(gse_eset_qn),sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_qn)
drawUMAP(t(gse_eset_qn),type)
drawUMAP(t(gse_eset_qn),as.factor(batch))
#######################################combat
batch<-sample_info$batch
modcombat = model.matrix(~as.factor(type), data=sample_info)

# #old
# system.time( 
#   combat_edata <- ComBat(gse_eset,batch,mod=NULL)
# )
# #0.53 0.11 0.64 
# pvcaobj_combat<-pvcaBF(t(combat_edata),sample_info,c("batch","type"),0.1)
# pieDraw(pvcaobj_combat)

#old
system.time( 
  combat_edata_mod <- ComBat(gse_eset_qn,batch,mod=modcombat)
)
#0.53 0.11 0.64 
pvcaobj_combat_mod<-pvcaBF(t(combat_edata_mod),sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_combat_mod)
drawUMAP(t(combat_edata_mod),as.factor(batch))
drawUMAP(t(combat_edata_mod),as.factor(type))

#old no
system.time( 
  combat_edata2 <- ComBat(gse_eset_qn,batch,mod=modcombat,par.prior=F)
)
#3195.69  203.70 3400.65 
pvcaobj_combat2<-pvcaBF(t(combat_edata2),sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_combat2)
drawUMAP(t(combat_edata2),as.factor(batch))
drawUMAP(t(combat_edata2),as.factor(type))

###auto
system.time( 
  combat_edata3 <- combat(gse_eset_qn,batch,mod=modcombat,par.prior="auto")
)
#3209.11  239.84 3452.00 
pvcaobj_combat3<-pvcaBF(t(combat_edata3$bayesdata),sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_combat3)
drawUMAP(t(combat_edata3$bayesdata),as.factor(batch))
drawUMAP(t(combat_edata3$bayesdata),as.factor(type))
drawPrior(combat_edata3$additiondata,"1")
drawPrior(combat_edata3$additiondata,"2")
###########################################################################################################
#random forest
source("RandomForest.R")
library(randomForest)
rownames(gse_eset_qn)<-rownames(gse_eset)
res.rf<-myRF2(gse_eset_qn,batch)
pdf("randomForest.pdf")
pvcaobj_rf<-pvcaBF(res.rf,sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_rf)
drawUMAP(res.rf,as.factor(batch))
drawUMAP(res.rf,as.factor(type))
dev.off()
######################################
d<-scale(gse_eset_qn, center = TRUE, scale = TRUE)
res.d<-myRF2(d,batch)
pvcaobj_rf2<-pvcaBF(res.d,sample_info,c("batch","type"),0.1)
pieDraw(pvcaobj_rf2)