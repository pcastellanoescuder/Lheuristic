

Selection <- 
  
  reactive({
    
    c.data <- cbind(metFile(), exprFile())
    
    x <- colnames(c.data)
    updateSelectInput(session,"one", choices = x[1:ncol(metFile())], selected = x[1])
    updateSelectInput(session,"two", choices = x[(ncol(metFile())+1):(ncol(metFile())+ncol(exprFile()))], 
                      selected = x[ncol(metFile())+1])
    print(c.data)
  })


Correlation_plot <- 
  
  reactive({
                    
                    c.data2 <- Selection()
                    
                    One <- as.character(input$one)
                    Two <- as.character(input$two)
                    
                    One.df <- as.data.frame(c.data2[, colnames(c.data2) == One])
                    
                    Two.df <- as.data.frame(c.data2[, colnames(c.data2) == Two])
                    
                    TOTAL <- cbind(One.df[1], Two.df[1])
                    colnames(TOTAL) <- c("Variable 1", "Variable 2")
                    
                    correlation_plot <- ggplot(TOTAL, aes(x = `Variable 1`, y = `Variable 2`)) + 
                                                   geom_point() +
                                                   xlab(One) + 
                                                   ylab(Two) + 
                                                   theme(legend.position="none") + 
                                                   theme_minimal()
                    
                    ####
                    
                    return(list(c.data2 = c.data2, correlation_plot = correlation_plot,
                                One = One, Two = Two, TOTAL = TOTAL))
                })


################# 

output$corr_plot <- renderPlot({
  
  c.data <- Correlation_plot()$c.data2
  
  c.data <- as.matrix(round(cor(c.data), 3))
  
  corrplot(c.data, method="color", type = "lower", tl.srt = 2)
  
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

output$corr_table <- renderDataTable({
  
  c.data <- Selection()
  
  flat_cor_mat <- function(cor_r, cor_p){
    cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
    cor_r <- gather(cor_r, column, cor, -1)
    cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
    cor_p <- gather(cor_p, column, p, -1)
    cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
    cor_p_matrix
  }
  
  c.data <- rcorr(as.matrix(c.data))
  
  my_cor_matrix <- flat_cor_mat(c.data$r, c.data$P)
  my_cor_matrix$fdr <- p.adjust(my_cor_matrix$p, method = "fdr")
  my_cor_matrix[, 3:5] <- round(my_cor_matrix[, 3:5], 4)
  my_cor_matrix <- my_cor_matrix[my_cor_matrix$row != my_cor_matrix$column ,]
  colnames(my_cor_matrix) <- c("Variable 1", "Variable 2", "correlation", "p.value", "fdr")
  my_cor_matrix <- my_cor_matrix[!duplicated(my_cor_matrix[, 1:2]) ,]
  
  datatable(my_cor_matrix, rownames = FALSE)

})
  
