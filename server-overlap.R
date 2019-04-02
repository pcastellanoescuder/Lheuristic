
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
    
    lshaped_genes <- rownames(seltable())
    
    intersection$`L-shape` <- NA
    
    for (i in 1:nrow(intersection)){
      if(intersection$Gene[i] %in% lshaped_genes){
        intersection$`L-shape`[i] <- TRUE
      } else{
        intersection$`L-shape`[i] <- FALSE
      }
    }
    
    
    lshaped_intersection <- intersection[intersection$`L-shape` == TRUE ,]
    
    intersection <- intersection[abs(intersection$Correlation) > input$rcoef ,]
    
    if (input$pval_type == "raw"){
      intersection <- intersection[intersection$p.value < input$pval ,]
    } 
    
    if (input$pval_type == "corrected"){
      intersection <- intersection[intersection$FDR < input$fdr ,]
    } 
    
    intersection <- rbind(intersection, lshaped_intersection)

    return(list(intersection = intersection))
    
  })

output$intersection_table <- renderDataTable({
  
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
                                    filename="intersection_table"),
                               list(extend="excel",
                                    filename="intersection_table"),
                               list(extend="pdf",
                                    filename="intersection_table")),
                  text="Dowload")),
              order=list(list(2, "desc")),
              pageLength = nrow(intersection)))
})

