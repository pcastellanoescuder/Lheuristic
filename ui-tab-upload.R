
fluidRow(
  column(width = 4,
         wellPanel(
           
  radioButtons("upload_type",  h3("Choose files:"),
               choices = c("Demo data" = 'demo',
                           "Upload data" = 'upload'),
               selected = 'demo'
  ),
  
  conditionalPanel(condition = ("input.upload_type == 'demo'"),
                   helpText("You can use pre-loaded data to see how this application works.",
                            "Just choose an example, set the paramaters as you wish",
                            "and click 'show genes'."),
                   helpText("If you want to work with your own data, click 'Upload data'",
                            "and upload your files."),
                   radioButtons('dataset', 'Choose a demo analysis',
                                c("GEO Dataset (All)"="GEOAll",
                                  "GEO Dataset (TRUE)"="GEOtrue"
                                ), selected="GEOAll")
  ),
  
  conditionalPanel(condition = ("input.upload_type == 'upload'"),

                   h3("Choose input files:"),
                   fileInput('metfile', 'Upload your methylation data',
                             accept = c('text/csv','.csv')),
                   p(strong('Set format parameters of your','methylation data file')),
                   fluidRow(
                     column(4,radioButtons('sep1', 'Separator',
                                           c(Tab='\t',
                                             Comma=',',
                                             Semicolon=';'),
                                           ';')),
                     column(4,radioButtons('dec1', 'Decimal',
                                           c(Point='.',
                                             Comma=','),
                                           ',')),
                     column(4,radioButtons('quote1', 'Quote',
                                           c(None='',
                                             'Double'='"',
                                             'Single'="'"),
                                           '"'))),
                   fileInput('exprfile', 'Upload your expression data',
                             accept = c('text/csv',
                                        '.csv')),
                   p(strong('Set format parameters of your','expression data file')),
                   fluidRow(
                     column(4,radioButtons('sep2', 'Separator',
                                           c(Tab='\t',
                                             Comma=',',
                                             Semicolon=';'),
                                           ';')),
                     column(4,radioButtons('dec2', 'Decimal',
                                           c(Point='.',
                                             Comma=','),
                                           ',')),
                     column(4,radioButtons('quote2', 'Quote',
                                           c(None='',
                                             'Double'='"',
                                             'Single'="'"),
                                           '"')
                     )
                   )
                   
  ))),
      
     column(width = 8,
            
                     fluidRow(
                       column(12,
                              h4("Methylation Data"),
                              tabPanel("Tabla", dataTableOutput("tab"))
                       ),
                       column(12,
                              
                              h4("Expression Data"),
                              tabPanel("Tabla", dataTableOutput("tab2"))
                       )
                     )
                     
            ))

