
Ngenes <- reactive({
  if(input$allgenes){
    Ngenes <- NULL
  }else{
    Ngenes <- input$Ngenes
  }
})

#Load user methylation data or demo methylation data
metFile <- reactive({
  
  metinFile <- input$metfile
  
  if (!is.null(metinFile)){
    metFile <- read.csv2(metinFile$datapath, header = TRUE,
                         sep = input$sep1, dec = input$dec1, quote = input$quote1, 
                         row.names = 1)
  }else{
    dataset <- input$dataset

    if(dataset=="GEOAll"){
      metFile <- read.table(file.path(dataDir, "GEOMethData.csv"), header = TRUE,
                            sep = ";", dec = ".", quote = '"', 
                            row.names = 1)
    }else{
      metFile <- read.table(file.path(dataDir, "GEOTrueLMetilacion.csv"), header = TRUE,
                            sep = ";", dec = ".", quote = '"', 
                            row.names = 1)
    }
  }
  
  return(metFile)
})

#####

metData <- reactive({
  
  Ngenes <- Ngenes()
  metFile <- metFile()
  
  if(!is.null(Ngenes)){
    Ngenes <- ifelse(Ngenes < nrow(metFile), Ngenes, nrow(metFile))
    metData <- metFile[1:Ngenes,]
  }else{
    metData <- metFile
  }
  
  return(metData)
})

#Load user expression data or demo expression data
exprFile <- reactive({
  
  exprinFile <- input$exprfile
  Ngenes <- Ngenes()
  
  if(!is.null(exprinFile)){
    exprFile <- read.csv2(exprinFile$datapath, header = TRUE,
                          sep = input$sep2, dec = input$dec2, quote = input$quote2, 
                          row.names = 1)
  }else{
    
    dataset <- input$dataset
    
    if(dataset=="GEOAll"){
      exprFile <- read.csv2(file.path(dataDir, "GEOExpData.csv"), header = TRUE,
                            sep = ";", dec = ".", quote = '"', 
                            row.names = 1)
    }else{
      exprFile <- read.csv2(file.path(dataDir, "GEOTrueLExpression.csv"), header = TRUE,
                            sep = ";", dec = ".", quote = '"', 
                            row.names = 1)
    }
  }
  return(exprFile)
})

#####

exprData <- reactive({
  
  Ngenes <- Ngenes()
  exprFile <- exprFile()
  
  if(!is.null(Ngenes)){
    Ngenes <- ifelse(Ngenes < nrow(exprFile), Ngenes, nrow(exprFile))
    exprData <- exprFile[1:Ngenes,]
  }else{
    exprData <- exprFile
  }
  
  return(exprData)
})

output$tab <- renderDataTable(metFile(), options = list(scrollX = TRUE, pageLength = 5))
output$tab2 <- renderDataTable(exprFile(), options = list(scrollX = TRUE, pageLength = 5))

