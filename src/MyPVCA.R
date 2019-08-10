pvcaBF<-function(df,sampleInfo,batch_effect,threshold){
  df<-data.matrix(t(df))
  phenoData <- new("AnnotatedDataFrame",data=sampleInfo)
  myExpressionSet <- ExpressionSet(assayData=df,
                                   phenoData=phenoData
  )
  pvcaObj <- pvcaBatchAssess (myExpressionSet, batch_effect, threshold)
  return(pvcaObj)
}
pvcaDraw<-function(pvcaobj){
  dat<-as.vector(unlist(pvcaobj$dat))
  label<-pvcaobj$label
  data<-data.frame(cbind(dat,label))
  p<-ggplot(data, aes(x=label, y=dat, fill=dat)) +
    geom_bar(stat="identity")+theme_minimal()+ theme(legend.position="none")
  print(p)
  p
}
