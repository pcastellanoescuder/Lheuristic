fluidRow(
  
  fluidPage(
    tabsetPanel(
      tabPanel("Venn Diagram", 
               plotOutput("venn_diagram")),
      
      tabPanel("Overlap Table", 
               dataTableOutput("intersection_table")),
      
      tabPanel("All Genes Table", 
               dataTableOutput("all_genes_table"))
      
    ))
)

