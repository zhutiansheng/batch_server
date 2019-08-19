function(input, output,session) {
  observe({
    effect_name<-colnames(getSampleInfo())
    adjust_variable<-effect_name
    # if(!is.null(input$batch_effect_name)){
    #   adjust_variable<-effect_name[-which(effect_name==isolate(input$batch_effect_name))]
    # 
    # }
      
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
    updateSelectInput(session, "batch_effect_name_rf",
                      choices = effect_name,
                      selected = NULL
    )
  })
  
  output$upload_note <- renderText({
    
    if(is.null(getMyd()))
      "Please upload data first"
    else paste("Sucessfully uploaded data dimension is",paste(dim(getMyd()),collapse = " × "),"and sample dimension is",paste(dim(getSampleInfo()),collapse = " × "))
    
  })
  
  getMyd<-eventReactive(input$input_submit, {
    withProgress(message = 'Read data in progress',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    print("data read")               
    myd<-read.table(input$myd$datapath,sep = input$sep,header = input$header,encoding = "UTF-8",check.names = F)  
    print(input$myd$datapath)
    df2<-myd[-1]
    rownames(df2)<-myd[,1]
    df2<-t(df2)
    incProgress(9/10,"Data read completed")
                 })
    return(df2)
  },ignoreNULL = T,ignoreInit =T
  )
  getSampleInfo<-eventReactive(input$input_submit, {
    print("Read sample")
    withProgress(message = 'Read sample in progress',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    myd<-read.table(input$sample_info$datapath,sep = input$sample_sep,header = input$sample_header,encoding = "UTF-8",check.names = F)  
    rownames(myd)<-myd[,1]
    myd<-myd[-1]
    incProgress(9/10,"Sample read completed")
                 })
    return(myd)
  },ignoreNULL = T,ignoreInit =T
  )
  #################################################################################
  ###pvca
  getPVCA<-eventReactive(input$pvca_submit,{
    withProgress(message = 'Read sample in progress',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    print("pvca start")
    pvcaobj<-pvcaBF(getMyd(),getSampleInfo(),input$pvca_effect_name,input$pvca_threshold)
    incProgress(9/10,"Prepare to return")
                 })
    print("pvca return")
    return(pvcaobj)
  },ignoreNULL = T,ignoreInit =T)
  output$draw_pvca<-renderPlot({
    withProgress(message = 'Read sample in progress',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    print(pvcaDraw(getPVCA()))
    incProgress(9/10,"PVCA plot completed")
  })
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
    withProgress(message = 'Calculating now',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    print("umap start")
    myd<-getMyd()
    myd[is.na(myd)]<-0
    myumap<-umap(myd,n_neighbors=input$n_neighbors)
    umap.layout<-data.frame(myumap$layout)
    incProgress(9/10,"Finished!")
    })           
    return(umap.layout)
  },ignoreNULL = T,ignoreInit =T
  )
  output$umap_note <- renderText({
    if(!is.null(getUmap()))
    "UMAP calculation is done. Select following to (re)draw UMAP"
  })
  
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
  output$draw_umap<-renderPlotly({
    plotly_build(drawUmap())
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
    withProgress(message = 'Calculating now',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
    print("Combat start")
    mod=NULL
    if(!is.null(input$adjust_variables)){
      pheno<-getSampleInfo()[input$adjust_variables]
      adjust_f<-paste0("as.factor(",input$adjust_variables,")",collapse = "+")
      mod<-model.matrix(eval(parse(text=paste('~', adjust_f))),data=pheno)
    }

    batch<-getSampleInfo()[,input$batch_effect_name]
    dat<-t(getMyd())
    dat<-as.matrix(dat)
    result<-combat(dat,batch , mod = mod, par.prior=input$par.prior, fit.method=input$fit.method,  
                      mean.only = input$mean.only, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 
    incProgress(9/10,"Completed")
                 })
    return(result)
  },ignoreNULL = T,ignoreInit =T)
  
  output$combat_log<-renderText({
    print(str(eliminateBF()))
    if(length(eliminateBF())<1)
      eliminateBF()
    else {
      batch_passTest<-eliminateBF()$additiondata$passTest
      batch<-as.factor(getSampleInfo()[,input$elimination])
      me<-paste0("Sucessfully adjusted ",nlevels(batch)," batches, ",
                 length(input$adjust_variables)," covariate variable(s). Parameter estimated batch(es): ",
                 paste(names(batch_passTest)[batch_passTest==TRUE],collapse = " "),". And noparameter estimated batch(es): ",
                 paste(names(batch_passTest)[batch_passTest==FALSE],collapse = " "))
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
        plotOutput("prior_plot"),
        verbatimTextOutput("prior_plot_note")
      )
    }
  })
  output$prior_plot<-renderPlot({
    drawPrior(eliminateBF()$additiondata,input$batch_para_name)
  })
  output$prior_plot_note<-renderText({
    "Note: blue lines/points means a kernel estimate of the empirical batch effect density while red as the parametric estimate."
  })
  output$cleanData_download <- downloadHandler(
    filename = function() {
      paste("BatchFree", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(eliminateBF()$bayesdata,file,row.names = T,quote = F,na="")
    }
  )
  #######################################################################33
  ##remove top n batch effect related feature
  getImpFeature<-eventReactive(input$rf_submit,{
    withProgress(message = 'Calculating now',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
                   print("random forest start")
                   label<-getSampleInfo()[,input$batch_effect_name_rf]
                   dat<-data.frame(getMyd())
                   #print(head(dat))
                   dat$label<-as.factor(label)
                   result<-myRF(dat,input$ntree,input$nodesize)
                   incProgress(9/10,"Completed")
                 })
    if(length(result)<1)
      return("Something wrong, please check your data!")
    return(result)
  },ignoreNULL = T,ignoreInit =T)
  
  output$rf_log<-renderText({
    if(length(getImpFeature())<1)
      getImpFeature()
    else {
      return(paste("Filter out top ",input$topN," variables: ",paste(getImpFeature()$features[1:input$topN],collapse = ",")))
    }
    
  })
  output$rf_ui <- renderUI({
    if(length(getImpFeature())>1){
      tagList(
        downloadButton("cleanData_download_rf", "getResult", class = "btn-primary"),
        plotOutput("model_plot_rf"),
        verbatimTextOutput("rf_plot_note")
      )
    }
  })
  output$model_plot_rf<-renderPlot({
    plot(getImpFeature()$mod,main="")
  })
  output$rf_plot_note<-renderText({
    "Note: Random Forest process graph"
  })
  output$cleanData_download_rf <- downloadHandler(
    filename = function() {
      paste("SubSet", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      features_remain<-setdiff(rownames(getMyd()),getImpFeature()$features[1:input$topN])
      
      write.csv(getMyd()[features_remain,],file,row.names = T,quote = F,na="")
    }
  )
}
