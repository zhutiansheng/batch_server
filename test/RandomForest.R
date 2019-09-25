myRF2<-function(d){
  RandomForest <- randomForest(label ~ . ,data=d,importance=T,na.action=na.exclude)
  
  imps  <- data.frame(importance(RandomForest));
  impScore <- imps$MeanDecreaseAccuracy
  #return(impScore)
  imps <- imps[order(impScore,decreasing=T),]
  orderedFeatures <- rownames(imps)
  d2<-d[,orderedFeatures[1:ceiling(0.1*nrow(d))]]
  return(d2)
}