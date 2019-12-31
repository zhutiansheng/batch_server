myRF<-function(d,ntree,nodesize){
  label_index<-grep("^label$",colnames(d))
  header<-colnames(d[-label_index])
  header_name<-paste0("index_",1:length(header))
  names(header)<-header_name
  colnames(d)[-label_index]<-header_name
  #d[is.na(d)]<-0
  #d$label<-as.character(d$label)
  RandomForest <- randomForest(label ~ . ,data=d,importance=T,ntree=ntree,nodesize=nodesize,na.action=na.exclude)
  
  imps  <- data.frame(importance(RandomForest));
  impScore <- imps$MeanDecreaseAccuracy
  #return(impScore)
  imps <- imps[order(impScore,decreasing=T),]
  orderedFeatures <- rownames(imps)
  orderedFeatures<-header[orderedFeatures]
  return(list(features=orderedFeatures,mod=RandomForest))
}

# myd<-read.table("../BatchServer2/testData/120min_os_prot_sr20191226a.csv",sep = ",",header = T)
# df2<-myd[-1]
# rownames(df2)<-myd[,1]
# df2<-t(df2)
# 
# 
# myd<-read.table("../BatchServer2/testData/patient_info_120min_sr20191226a.csv",sep = ",",header = T)
# rownames(myd)<-myd[,1]
# myd<-myd[-1]
# sample<-myd
# 
# label<-sample[,"Batch_ID"]
# dat<-data.frame(df2,check.names = F)
# dat$label<-as.factor(label)
# 
# cna<-apply(dat,2,function(x){sum(is.na(x))})
# d<-dat[,cna<10]
# ntree=500
# nodesize=5
