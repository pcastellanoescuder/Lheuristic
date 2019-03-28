

Selection <- 
  
  reactive({
    
    c.data <- metFile()
    
    x <- colnames(c.data)
    updateSelectInput(session,"one", choices = x, selected = x[1])

    print(c.data)
  })


Correlation_plot <- 
  
  reactive({
    
    met <- metFile()
    exp <- exprFile()
                    
                    One <- as.character(input$one)

                    One.df <- met[, colnames(met) == One]
                    
                    Two.df <- exp[, colnames(exp) == One]
                    
                    TOTAL <- as.data.frame(cbind(One.df, Two.df))
                    colnames(TOTAL) <- c("Gene Methylation", "Gene Expression")
                    
                    correlation_plot <- ggplot(TOTAL, aes(x = `Gene Methylation`, y = `Gene Expression`)) + 
                                                   geom_point() +
                                                   xlab("Gene Methylation") + 
                                                   ylab("Gene Expression") + 
                                                   theme(legend.position="none") + 
                                                   theme_minimal()
                    
                    ####
                    
                    return(list(correlation_plot = correlation_plot, One = One, TOTAL = TOTAL))
                })


################# 

output$corr_plot <- renderPlot({
  
  met <- metFile()
  exp <- exprFile()
  
  rownames(met) <- paste0(rownames(met), "_MET")
  rownames(exp) <- paste0(rownames(exp), "_EXP")
  
  c.data <- cbind(met, exp)
  
  c.data <- as.matrix(round(cor(c.data), 3))
  
  corrplot(c.data, method="color", type = "lower", tl.srt = 2)
  
})

output$cor_plot <- renderPlot({
  Correlation_plot()$correlation_plot
})

output$text <- renderText({
  One <- Correlation_plot()$One
  TOTAL <- Correlation_plot()$TOTAL
    
  paste0("The ",input$corr_method," correlation between methylation and expression in ", One," gene is ",
         round(cor(TOTAL$`Gene Methylation`, TOTAL$`Gene Expression`, method = input$corr_method),3),
         " and p-value is ", 
         round(cor.test(TOTAL$`Gene Methylation`, TOTAL$`Gene Expression`, 
                        method = input$corr_method)$p.value,3))
})


output$corr_table <- renderDataTable({
  
  met <- metFile()
  exp <- exprFile()
  
  c.data <- cbind(met, exp)
  
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
  
