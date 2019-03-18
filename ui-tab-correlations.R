
fluidRow(
  column(width = 3,
                wellPanel(
  
                  h4("Correlation between:"),
                  
                  selectInput("one",label="Variable 1", choices = NULL),
                  
                  h4("and"),
                  
                  selectInput("two",label="Variable 2", choices = NULL), 
                  
                  radioButtons("corr_method", "Correlation Method:", c("Pearson" = "pearson",
                                                                       "Spearman" = "spearman",
                                                                       "Kendall" = "kendall"))
  )),
  
  column(width = 8,
         
         fluidPage(
           tabsetPanel(
             tabPanel("Pairwise Correlation Scatterplot", 
                      plotOutput("cor_plot"),
                      br(),
                      textOutput("text")),
             tabPanel("Global Correlation Plot", plotOutput("corr_plot", height = 700)),
             tabPanel("Table of Correlations", dataTableOutput("corr_table"))
           ))

  ))

