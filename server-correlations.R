

Selection <- 
  
  reactive({
    
    c.data <- cbind(metFile(), exprFile())
    
    x <- colnames(c.data)
    updateSelectInput(session,"one", choices = x[1:ncol(metFile())], selected = x[1])
    updateSelectInput(session,"two", choices = x[(ncol(metFile())+1):(ncol(metFile())+ncol(exprFile()))], selected = x[ncol(metFile())+1])
    print(c.data)
  })


Correlation_plot <- 
  reactive({
                    
                    c.data <- Selection()
                    c.data <- as.matrix(round(cor(c.data), 3))
                    
                    ####
                    
                    c.data2 <- Selection()
                    
                    One <- as.character(input$one)
                    Two <- as.character(input$two)
                    
                    One.df <- as.data.frame(c.data2[,colnames(c.data2) == One])
                    
                    Two.df <- as.data.frame(c.data2[,colnames(c.data2) == Two])
                    
                    TOTAL <- cbind(One.df, Two.df)
                    colnames(TOTAL) <- c("Variable 1", "Variable 2")
                    
                    correlation_plot <- ggplot(TOTAL, aes(x = `Variable 1`, y = `Variable 2`)) + 
                                                   geom_point() +
                                                   xlab(One) + 
                                                   ylab(Two) + 
                                                   theme(legend.position="none") + 
                                                   theme_minimal()
                    
                    ####
                    
                    return(list(c.data = c.data, correlation_plot = correlation_plot,
                                One = One, Two = Two, TOTAL = TOTAL))


                })


################# 

output$corr_plot <- renderPlot({
  
  c.data <- Correlation_plot()$c.data
  
  corrplot(c.data, method="color")
  
})

output$cor_plot <- renderPlot({
  Correlation_plot()$correlation_plot
})

output$text <- renderText({
  One <- Correlation_plot()$One
  Two <- Correlation_plot()$Two
  TOTAL <- Correlation_plot()$TOTAL
    
  paste0("The ",input$corr_method," correlation between ", One," and ", Two, " is ",
         round(cor(TOTAL$`Variable 1`, TOTAL$`Variable 2`, method = input$corr_method),3),
         " and p-value is ", 
         round(cor.test(TOTAL$`Variable 1`, TOTAL$`Variable 2`, 
                        method = input$corr_method)$p.value,3))
})

