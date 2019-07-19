
fluidRow(
  column(width = 3,
                wellPanel(
  
                  h3("Select Gene:"),
                  
                  selectInput("one", label = "Gene", choices = NULL),
                  
                  checkboxInput("smooth", "Smooth line (lm)"),
                  conditionalPanel(condition = ("input.smooth"),
                                   selectInput("smooth_color", "Smooth line colour", choices = c("red", "blue", "green"))),
                  
                  radioButtons("corr_method", "Correlation Method:", c("Pearson" = "pearson",
                                                                       "Spearman" = "spearman",
                                                                       "Kendall" = "kendall")),
                  
                  h3("Select Table Parameters:"),
                  
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
           tabsetPanel(
             tabPanel("Pairwise Correlation Scatterplot", 
                      br(),
                      downloadButton("download_plot", "Download Plot"),
                      br(),
                      br(),
                      plotOutput("cor_plot"),
                      br(),
                      textOutput("text")),
             tabPanel("Global Correlation Plot", 
                      br(),
                      downloadButton("download_plot2", "Download Plot"),
                      br(),
                      br(),
                      plotOutput("corr_plot", height = 700)),
             tabPanel("Table of Correlations", dataTableOutput("corr_table"))
           ))

  ))

