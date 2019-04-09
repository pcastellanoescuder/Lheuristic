

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

