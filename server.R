shinyServer(function(input, output) {
  model <- reactive({
  lda <- LDA(dtm.new,input$K)
  Terms <- data.frame(terms(lda))
  Terms$Topic <- 1:nrow(Terms)
  res <- data.frame(df[rowTotals>0,]$text,topics(lda))
  names(res) <- c('Tweet','Topic')
  df <- merge(res,Terms,'Topic',all.x=T)
  names(df)[3] <- 'Keyword'
  df
  })

  
  output$g <- renderPlot({
    dat <- model()
    agg <- data.table(dat)[,list(.N),by=list(Topic)]
    agg <- unique(merge(agg,dat[-2],'Topic',all.x=T))
    agg$Topic <- as.factor(as.character(agg$Topic))
    g <- ggplot(agg)+
      geom_bar(aes(x=Topic,y=N,fill=Topic),stat="identity")+
      geom_text(aes(x=Topic,y=N+5,label=Keyword,color=Topic))+
      labs(x="Topic",y='Number of tweets',title='Topics and keywords')
    print(g)
  })  
  
output$tabl <- renderDataTable({
  model()
}, options = list(bSortClasses = TRUE,aLengthMenu = c(10, 30, 100), iDisplayLength = 10))



})