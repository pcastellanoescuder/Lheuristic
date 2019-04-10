

output$venn_diagram <- renderPlot({
  
  num_cor <- nrow(Intersection()$intersection)
  num_l <- nrow(seltable())
  
  l_names <- rownames(seltable())
  c_names <- Intersection()$intersection[,1]
  
  num_overlap <- length(intersect(l_names, c_names))
  
  df.venn <- data.frame(x = c(0, 2),
                        y = c(0, 0),
                        Labels = c('  Correlated Genes  ',
                                   '  L-shaped Genes  '))
  
  ggplot(df.venn, aes(x0 = x, y0 = y, r = 1.5, fill = Labels)) +
    geom_circle(alpha = .3, size = 1, colour = 'grey') +
    coord_fixed() +
    theme_void() +
    annotate("text", x = c(-0.5, 1, 2.5), y = c(0, 0, 0), label = c(num_cor, num_overlap, num_l), size = 6) + 
    theme(legend.position="bottom") + 
    theme(legend.title = element_blank()) + 
    theme(legend.text=element_text(size=15))
  
})

output$intersection_table <- renderDataTable({

  l_names <- rownames(seltable())
  c_names <- Intersection()$intersection[,1]
  
  names_overlap <- intersect(l_names, c_names)
  
  met <- metFile()
  met <- as.data.frame(t(met))
  
  exp <- exprFile()
  exp <- as.data.frame(t(exp))
  
  intersection_table <- data.frame(Gene = rep(NA, ncol(met)), Correlation = rep(NA, ncol(met)), p.value = rep(NA, ncol(met)))
  
  for (i in 1:ncol(met)){
    intersection_table$Gene[i] <- colnames(met)[i]
    intersection_table$Correlation[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$estimate,3)
    intersection_table$p.value[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$p.value,3)
  }
  
  intersection_table$FDR <- round(p.adjust(intersection_table$p.value, method = "fdr"),3)
  
  intersection_table <- intersection_table[intersection_table$Gene %in% names_overlap ,]
  
  datatable(intersection_table, 
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
              pageLength = nrow(intersection_table)))
})

output$all_genes_table <- renderDataTable({
  
  l_names <- rownames(seltable())
  c_names <- Intersection()$intersection[,1]

  met <- metFile()
  met <- as.data.frame(t(met))
  
  exp <- exprFile()
  exp <- as.data.frame(t(exp))
  
  all_genes_table <- data.frame(Gene = rep(NA, ncol(met)), Correlation = rep(NA, ncol(met)), p.value = rep(NA, ncol(met)))
  
  for (i in 1:ncol(met)){
    all_genes_table$Gene[i] <- colnames(met)[i]
    all_genes_table$Correlation[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$estimate,3)
    all_genes_table$p.value[i] <- round(cor.test(met[, colnames(met)[i]], exp[, colnames(exp) == colnames(met)[1]])$p.value,3)
  }
  
  all_genes_table$FDR <- round(p.adjust(all_genes_table$p.value, method = "fdr"),3)
  
  all_genes_table$`L-shape` <- NA
  
  for (i in 1:nrow(all_genes_table)){
    if(all_genes_table$Gene[i] %in% l_names){
      all_genes_table$`L-shape`[i] <- TRUE
    } else{
      all_genes_table$`L-shape`[i] <- FALSE
    }
  }

  datatable(all_genes_table, 
            filter = 'none',extensions = 'Buttons',
            escape=FALSE,  rownames=FALSE, class = 'cell-border stripe',
            options = list(
              dom = 'Bfrtip',
              buttons = 
                list("copy", "print", list(
                  extend="collection",
                  buttons=list(list(extend="csv",
                                    filename="all_genes_table"),
                               list(extend="excel",
                                    filename="all_genes_table"),
                               list(extend="pdf",
                                    filename="all_genes_table")),
                  text="Dowload")),
              order=list(list(2, "desc")),
              pageLength = nrow(all_genes_table)))
})

