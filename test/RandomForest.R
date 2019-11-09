myRF2<-function(df,label){
  rownames(df)<-paste0("A",rownames(df))
  df<-t(df)
  label<-as.factor(label)
  RandomForest <- randomForest(df,label,importance=T,na.action=na.exclude,nodesize = 1)
  
  imps  <- data.frame(importance(RandomForest));
  impScore <- imps$MeanDecreaseAccuracy
  #return(impScore)
  imps <- imps[order(impScore,decreasing=T),]
  orderedFeatures <- rownames(imps)
  #orderedFeatures<-gsub("^A","",orderedFeatures)
  
  d2<-df[,orderedFeatures[ceiling(0.8*ncol(df)):ncol(df)]]
  colnames(d2)<-gsub("^A","",colnames(d2))
  return(d2)
}
