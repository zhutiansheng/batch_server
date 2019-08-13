pvcaBF<-function(df,sampleInfo,batch_effect,threshold){
  print("pvcaBF start")
  print(head(df))
  df<-data.matrix(t(df))
  print(head(df))
  phenoData <- new("AnnotatedDataFrame",data=sampleInfo)
  myExpressionSet <- ExpressionSet(assayData=df,
                                   phenoData=phenoData
  )
  pvcaObj <- pvcaBatchAssess (myExpressionSet, batch_effect, threshold)
  print("pvcaBF end")
  return(pvcaObj)
}
pvcaDraw<-function(pvcaobj){
  dat<-as.vector(unlist(pvcaobj$dat))
  label<-pvcaobj$label
  data<-data.frame(cbind(dat,label))
  data$dat<-as.numeric(data$dat)
  p<-ggplot(data, aes(x=label, y=dat, fill="LightSeaGreen")) +
    geom_bar(stat="identity")+theme(legend.position="none")
  p<-p+theme(axis.text.x = element_text(vjust = 0.001,angle = 45))+ylim(0,1)+
    labs(x='Source of variance', y='Weighted average proportion variance')
  p
}
pieDraw<-function(pvcaobj){
  dat<-as.vector(unlist(pvcaobj$dat))
  Source<-pvcaobj$label
  df<-data.frame(cbind(dat,Source))
  df$dat<-as.numeric(df$dat)
  df$percent<-round(df$dat/sum(df$dat)*100,3)
  df$Source<-paste0(Source," (",df$percent,")")
  p<-ggplot(df, aes(x = "", weight =percent , fill = label)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") + xlab('') + ylab('') +
    #geom_text(aes(x = 1.3, y = Data, label = Sample))+
    theme_minimal()  
  p
}