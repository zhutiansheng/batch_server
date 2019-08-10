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
  p<-ggplot(data, aes(x=label, y=dat, fill="LightSeaGreen")) +
    geom_bar(stat="identity")+theme(legend.position="none")
  p<-p+theme(axis.text.x = element_text(vjust = 0.01,angle = 45))+
    labs(x='Source of variance', y='Weighted average proportion variance')
  p
}
