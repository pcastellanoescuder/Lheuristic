fluidRow(
  
  fluidPage(
    tabsetPanel(
      tabPanel("Venn Diagram", 
               plotOutput("venn_diagram")),
      
      tabPanel("Intersection Table", 
               dataTableOutput("intersection_table")),
      
      tabPanel("All Genes Table", 
               dataTableOutput("all_genes_table"))
      
    ))
)

