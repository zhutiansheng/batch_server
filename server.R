function(input, output,session) {
  observe({
    effect_name<-colnames(getSampleInfo())
    adjust_variable<-""
    if(!is.null(input$batch_effect_name)){
      adjust_variable<-effect_name[-which(effect_name==input$batch_effect_name)]
      
    }
      
    updateSelectInput(session, "pvca_effect_name",
                      choices = effect_name,
                      selected = NULL
    ) 
    updateSelectInput(session, "umap_effect_name",
                      choices = effect_name,
                      selected = NULL
    ) 
    updateSelectInput(session, "batch_effect_name",
                      choices = effect_name,
                      selected = NULL
    ) 
    updateSelectInput(session, "adjust_variables",
                      choices = adjust_variable,
                      selected = NULL
    )
  })
  getMyd<-eventReactive(input$input_submit, {
    myd<-read.table(input$myd$datapath,sep = input$sep,header = input$header,encoding = "UTF-8",check.names = F)  
    df2<-myd[-1]
    rownames(df2)<-myd[,1]
    df2<-t(df2)
  },ignoreNULL = T,ignoreInit =T
  )
  getSampleInfo<-eventReactive(input$input_submit, {
    myd<-read.table(input$sample_info$datapath,sep = input$sample_sep,header = input$sample_header,encoding = "UTF-8",check.names = F)  
    rownames(myd)<-myd[,1]
    myd<-myd[-1]
    myd
  },ignoreNULL = T,ignoreInit =T
  )
  #################################################################################
  ###pvca
  getPVCA<-eventReactive(input$pvca_submit,{
    print("pvca start")
    pvcaobj<-pvcaBF(getMyd(),getSampleInfo(),input$pvca_effect_name,input$pvca_threshold)
    return(pvcaobj)
  },ignoreNULL = T,ignoreInit =T)
  output$draw_pvca<-renderPlot({
    pvcaDraw(getPVCA())
  })
  output$pvca_ui <- renderUI({
    if(!is.null(getPVCA()))
      downloadButton("pvca_download", "Download", class = "btn-primary")
  })
  output$pvca_download <- downloadHandler(
    filename = function() {
      paste("pvca_barplot", Sys.time(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(pvcaDraw(getPVCA()))
      dev.off()
      
    }
  )
  ##piePlot
  output$draw_pie<-renderPlot({
    pieDraw(getPVCA())
  })
  output$pvca_pie_ui <- renderUI({
    if(!is.null(getPVCA()))
      downloadButton("pvca_pie_download", "Download", class = "btn-primary")
  })
  output$pvca_pie_download <- downloadHandler(
    filename = function() {
      paste("pvca_pie", Sys.time(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(pieDraw(getPVCA()))
      dev.off()
      
    }
  )
  #################################################################################
  ###umap
  getUmap<-eventReactive(input$umap_submit, {
    print("umap start")
    myd<-getMyd()
    myd[is.na(myd)]<-0
    myumap<-umap(myd,n_neighbors=input$n_neighbors)
    umap.layout<-data.frame(myumap$layout)
    
    return(umap.layout)
  },ignoreNULL = T,ignoreInit =T
  )
  drawUmap<-eventReactive(input$umap_effect_name,{
    mydf<-data.frame(getUmap())
    mydf$label<-getSampleInfo()[rownames(getMyd()),input$umap_effect_name]
    p<-ggplot(mydf,aes(x=X1, y=X2, colour=label)) + geom_point(size=3)+
      theme(  #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #axis.text.x = element_text(vjust = 1,angle = 45),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank())
    return(p)
    
  },ignoreNULL = T,ignoreInit =T)
  output$draw_umap<-renderPlot({
    drawUmap()
  })
  output$umap_ui <- renderUI({
    if(!is.null(drawUmap()))
      downloadButton("umap_download", "Download", class = "btn-primary")
  })
  output$umap_download <- downloadHandler(
    filename = function() {
      paste("umap", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(drawUmap())
      dev.off()
      
    }
  )
  #######################################################################33
  ##remove batch effect
  eliminateBF<-eventReactive(input$elimination_submit,{
    mod=NULL
    if(!is.null(input$adjust_variables)){
      pheno<-getSampleInfo()[input$adjust_variables]
      adjust_f<-paste0("as.factor(",input$adjust_variables,")",collapse = "+")
      mod<-model.matrix(eval(parse(text=paste('~', adjust_f))),data=pheno)
    }
    batch<-getSampleInfo()[input$elimination]
    
    combat(getMyd(),batch , mod = mod, par.prior=input$par.prior, fit.method=input$fit.method,  
                      mean.only = input$mean.only, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 
    
  },ignoreNULL = T,ignoreInit =T)
  
  output$combat_log<-renderPrint({
    batch_passTest<-eliminateBF()$additiondata$passTest
    if(length(eliminateBF())>1)
      return(NULL)
    else {
      batch<-as.factor(getSampleInfo()[input$elimination])
      me<-paste0("Sucessfully adjusted ",nlevels(batch)," batches, ",
                 length(input$adjust_variables)," covariate variable(s). Parameter estimate batche(s): ",
                 names(batch_passTest)[batch_passTest==TRUE],". And noparameter estimate batche(s): ",
                 names(batch_passTest)[batch_passTest==FALSE])
      return(me)
    }
    
    })
  output$combat_ui <- renderUI({
    if(length(eliminateBF())>1){
      tagList(
        downloadButton("cleanData_download", "getResult", class = "btn-primary"),
        selectInput("batch_para_name","Select batch level to show whether it is reasonable",
                    choices = names(eliminateBF()$additiondata$passTest),
                    selected = NULL),
        plotOutput("prior_plot")
      )
    }
  })
  output$prior_plot<-renderPlot({
    drawPrior(eliminateBF()$additiondata,input$batch_para_name)
  })
  
  output$cleanData_download <- downloadHandler(
    filename = function() {
      paste("pvca", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(eliminateBF()$bayesdata,file,row.names = F,quote = F,na="")
    }
  )
}
