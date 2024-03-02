function(input,output) {
  
  ####
  
  datasetInput <- reactive({
    if (input$Region == "kothrud"){
      dataset <- kothrud
    }
    else if (input$Region == "sinhgad"){
      dataset <- sinhgad
    }
    else if (input$Region == "kasba"){
      dataset <- kasba
    }
    else if (input$Region == "yerawda"){
      dataset <- yerawda
    }
    else if (input$Region == "bibwewadi"){
      dataset <- bibwewadi
    }
    return(dataset)
  })
  
  
  ####
  
  
  
  data<-reactive({
    reg_qt<-subset(datasetInput(), 
                   Year==input$Year)
    kothrud_year<-aggregate(Amount~Expense.Code,data=reg_qt,FUN=sum)
    kothrud_year$TotalExpenses<-kothrud_year$Amount
    kothrud_year$Yearlytotal<-sum(kothrud$Amount)
    return(kothrud_year)
    })
  output$hist<-renderPlot({ 
    data <- data()
    
    hist <- ggplot(data = datasetInput(),mapping = aes(x=Year,y=Amount))+
      geom_bar(stat = "identity",fill="maroon",orientation = "x")+
      labs(x="Year",y="Amount Spent in lakhs")+
      theme_classic(base_size = 17)
    return(hist)
    
  })
  
  output$bar<-renderPlot({
    data<-data()
    graph_bar<-ggplot(data, aes(x = Expense.Code, y = TotalExpenses)) +
      geom_bar(stat = "identity",
               show.legend = F,
               fill=4) + # Remove the legend
      xlab("Expenditure")+
      ylab("Amount Spent in lakhs")+
      coord_flip()+
      theme_classic(base_size = 17)
      
    return(graph_bar)
    
  })
  
  ####
  
  output$summary <- renderPrint({
    summary(datasetInput()[3])
  })
  
  ####
  ###
  # 
  # output$summary <- renderTable({
  #   datasetInput()
  # })
  # 
  ###
  
}