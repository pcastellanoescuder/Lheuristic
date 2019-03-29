
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
                                                                       "Kendall" = "kendall"))
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

