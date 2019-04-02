fluidRow(
  column(width = 4,
         wellPanel(
           
           h3("Select intersection table parameters:"),
           
           numericInput("rcoef", "Select genes with a 
                       correlation coeficient (absolut value) above:", min = 0, max = 1, value = 0.7),
           
           selectInput("pval_type", "Which p value do you want to use for filter?", 
                       choices = c("None" = "none", "Raw p value" = "raw", "Corrected p value" = "corrected"),
                       selected = "none"),
           
           conditionalPanel("input.pval_type == 'raw'",
                            numericInput("pval", "Select genes with a raw p value below:",
                                         min = 0, max = 1, value = 0.01)),
           
           conditionalPanel("input.pval_type == 'corrected'",
                            numericInput("fdr", "Select genes with an adjusted p value (fdr) below:",
                                         min = 0, max = 1, value = 0.05))
           
)),
  
  column(width = 8,
         
         fluidPage(
           
           dataTableOutput("intersection_table")
           
           ))
)
         


