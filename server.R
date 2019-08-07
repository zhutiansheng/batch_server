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
    t(myd)
    }
  )
  getSampleInfo<-eventReactive(input$input_submit, {
    myd<-read.table(input$sample_info$datapath,sep = input$sample_sep,header = input$sample_header,encoding = "UTF-8",check.names = F)  
    }
  )
  getUmap<-eventReactive(input$umap_submit, {
      print("umap start")
      myd<-getMyd()
      print(head(myd))
      myumap<-umap(myd)
      umap.layout<-data.frame(myumap$layout)
      output$draw_umap<-renderPlot({
        print(head(umap.layout))
        plot(umap.layout)
      })
      return(umap.layout)
  }
  )

}