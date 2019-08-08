function(input, output,session) {
  observe({
    effect_name<-colnames(getSampleInfo())
    updateSelectInput(session, "pvca_effect_name",
                      choices = effect_name,
                      selected = NULL
                      ) 
    updateSelectInput(session, "umap_effect_name",
                      choices = effect_name,
                      selected = NULL
    ) 
  })
  getMyd<-eventReactive(input$input_submit, {
    myd<-read.table(input$myd$datapath,sep = input$sep,header = input$header,encoding = "UTF-8",check.names = F)  
    df2<-myd[-1]
    rownames(df2)<-myd[,1]
    df2<-t(df2)
    }
  )
  getSampleInfo<-eventReactive(input$input_submit, {
    myd<-read.table(input$sample_info$datapath,sep = input$sample_sep,header = input$sample_header,encoding = "UTF-8",check.names = F)  
    rownames(myd)<-myd[,1]
    myd
    }
  )
  getUmap<-eventReactive(input$umap_submit, {
      print("umap start")
      myd<-getMyd()
      myumap<-umap(myd)
      umap.layout<-data.frame(myumap$layout)
      
      return(umap.layout)
  }
  )
  drawUmap<-eventReactive(input$umap_effect_name,{
      mydf<-data.frame(getUmap())
      mydf$label<-getSampleInfo()[rownames(getMyd()),input$umap_effect_name]
      p<-ggplot(mydf,aes(x=X1, y=X2, colour=label)) + geom_point(size=3)+
        theme(  #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.background = element_blank())
      return(p)
    
  },ignoreNULL = T,ignoreInit =T)
  output$draw_umap<-renderPlot({
    drawUmap()
  })
  output$umap_ui <- renderUI({
    downloadButton("umap_download", "Download", class = "btn-primary")
  })
  output$umap_download <- downloadHandler(
    filename = function() {
      paste("umap", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      drawUmap()
      dev.off()
      
    }
  )
}