
observe({
  
  c.data <- metFile()
  
  c.data <- as.data.frame(t(c.data))
  
  x <- colnames(c.data)
  updateSelectInput(session,"one", choices = x, selected = x[1])
})


Correlation_plot <- 
  
  reactive({
    
    met <- metFile()
    met <- as.data.frame(t(met))
    
    exp <- exprFile()
    exp <- as.data.frame(t(exp))
                    
                    One <- as.character(input$one)

                    One.df <- met[, colnames(met) == One]
                    
                    Two.df <- exp[, colnames(exp) == One]
                    
                    TOTAL <- as.data.frame(cbind(One.df, Two.df))
                    colnames(TOTAL) <- c("Gene Methylation", "Gene Expression")
                    
                    if(input$smooth == TRUE){
                      
                      correlation_plot <- 
                        ggplot(TOTAL, aes(x = `Gene Methylation`, y = `Gene Expression`)) + 
                        geom_point(size = 3) +
                        xlab(paste0(One, " Gene Methylation")) + 
                        ylab(paste0(One, " Gene Expression")) + 
                        theme_minimal() + 
                        geom_smooth(method=lm, color=input$smooth_color) +
                        geom_rug() +
                        theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold"),
                              legend.position="none")
                      
                    } else {
                      
                      correlation_plot <- 
                        ggplot(TOTAL, aes(x = `Gene Methylation`, y = `Gene Expression`)) + 
                        geom_point(size = 3) +
                        xlab(paste0(One, " Gene Methylation")) + 
                        ylab(paste0(One, " Gene Expression")) + 
                        theme_minimal() + 
                        geom_rug() +
                        theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold"),
                              legend.position="none")
                      
                    }
                    
                    ####
                    
                    return(list(correlation_plot = correlation_plot, One = One, TOTAL = TOTAL))
                })


################# 

output$corr_plot <- renderPlot({
  
  met <- metFile()
  met <- as.data.frame(t(met))
  
  exp <- exprFile()
  exp <- as.data.frame(t(exp))
  
  colnames(met) <- paste0(colnames(met), "_MET")
  colnames(exp) <- paste0(colnames(exp), "_EXP")
  
  c.data <- cbind(met, exp)
  
  c.data <- as.matrix(round(cor(c.data), 3))
  
  corrplot(c.data, method="color", type = "lower", tl.srt = 2, tl.cex = 0.6)
  
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

output$download_plot <- downloadHandler(
  filename =  function() {
    paste0("Pairwise_correlation_", Sys.Date())
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
      pdf(file) # open the pdf device

    print(Correlation_plot()$correlation_plot) # for GGPLOT
    dev.off()  # turn the device off
    
  }) 

output$download_plot2 <- downloadHandler(
  filename =  function() {
    paste0("Global_correlations_", Sys.Date())
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    
    met <- metFile()
    met <- as.data.frame(t(met))
    exp <- exprFile()
    exp <- as.data.frame(t(exp))
    colnames(met) <- paste0(colnames(met), "_MET")
    colnames(exp) <- paste0(colnames(exp), "_EXP")
    c.data <- cbind(met, exp)
    c.data <- as.matrix(round(cor(c.data), 3))
    
    print(corrplot(c.data, method="color", type = "lower", tl.srt = 2, tl.cex = 0.6)) # for GGPLOT
    dev.off()  # turn the device off
    
  }) 

output$corr_table <- renderDataTable({

  
  datatable(my_cor_matrix, rownames = FALSE)
  
})

Intersection <- 
  
  reactive({
    
    met <- metFile()
    met <- as.data.frame(t(met))
    
    exp <- exprFile()
    exp <- as.data.frame(t(exp))
    
    intersection <- data.frame(Gene = rep(NA, ncol(met)), Correlation = rep(NA, ncol(met)), p.value = rep(NA, ncol(met)))
    
    for (i in 1:ncol(met)){
      intersection$Gene[i] <- colnames(met)[i]
      intersection$Correlation[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$estimate,3)
      intersection$p.value[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$p.value,3)
    }
    
    intersection$FDR <- round(p.adjust(intersection$p.value, method = "fdr"),3)
    
    intersection <- intersection[abs(intersection$Correlation) > input$rcoef ,]
    
    if (input$pval_type == "raw"){
      intersection <- intersection[intersection$p.value < input$pval ,]
    } 
    
    if (input$pval_type == "corrected"){
      intersection <- intersection[intersection$FDR < input$fdr ,]
    } 
    
    return(list(intersection = intersection))
    
  })

output$corr_table <- renderDataTable({
  
  intersection <- Intersection()$intersection
  
  datatable(intersection, 
            filter = 'none',extensions = 'Buttons',
            escape=FALSE,  rownames=FALSE, class = 'cell-border stripe',
            options = list(
              dom = 'Bfrtip',
              buttons = 
                list("copy", "print", list(
                  extend="collection",
                  buttons=list(list(extend="csv",
                                    filename="correlation_table"),
                               list(extend="excel",
                                    filename="correlation_table"),
                               list(extend="pdf",
                                    filename="correlation_table")),
                  text="Dowload")),
              order=list(list(2, "desc")),
              pageLength = nrow(intersection)))
})

