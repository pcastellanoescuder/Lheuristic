library(shiny)

shinyUI(navbarPage("Selection of L-shaped genes using a heuristic algorithm",
                   tabPanel("Home"),

                   tabPanel("L-heuristic",
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Demo data", br(),br(),
                 helpText("You can use pre-loaded data to see how this application works.",
                   "Just choose an example and go to the 'settings' tab, set the paramaters as you wish,",
                   "and click 'show genes'."),
                 helpText("If you want to work with your own data, go to the 'upload data'",
                   "tab and upload your files. Once your data is loaded, go to the 'settings' tab."),
                 radioButtons('dataset', 'Choose a demo analysis',
                              c(#"DA1 (Expression microarrays and methylation array)"="DA1", 
                                #"DA2 (Expression RNAseq and methylation array)"="DA2",
                                "GEO Dataset (All)"="GEOAll",
                                "GEO Dataset (TRUE)"="GEOtrue"
                                #, "TCGA Microarray", "TCGA RNAseq"
                                ), selected="GEOAll")
                 ),
        
        tabPanel("Upload data",
                 h3("Choose input files"),
                 fileInput('metfile', 'Upload your methylation array',
                           accept = c('text/csv','.csv')),
                 p(strong('Set format parameters of your',
                          'methylation data file')),
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
                 fileInput('exprfile', 'Upload your expression microarray or RNAseq',
                           accept = c('text/csv',
                                      '.csv')),
                 p(strong('Set format parameters of your',
                          'expression data file')),
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
                 ),
        
        tabPanel("Settings",
                 h3("Select L-shape parameters", align = "left"),
                 fluidRow(column(8,
                                 numericInput("Ngenes",label = "Number of genes to analyse", 
                                              value = 200, min = 0)),
                          column(4,br(),
                                 checkboxInput("allgenes", label="select all"), value=FALSE)),
                 fluidRow(# column( 7,
                                 # radioButtons('scoreFunc', 'Compute scores by:',
                                 #            c('Three bands rule' = 'scoreGenesMat',
                                 #               'Nine cells rule'='scoreGenesMat2'))),
                          column(7,# br(),
                                 actionButton("submit","Select genes")
								 )
						),br(),



                                 
                 sliderInput("xaxis", "Coordinates of vertical points in the x-axis",
                             min = 0, max = 1, value = c(1/3, 2/3)),
                 sliderInput("yaxis", "Coordinates of vertical points in the y-axis",
                             min = 0, max = 1, value = c(1/3, 2/3)),
                 
                 p(strong("Minimum counts per cell (%)")),
                 fluidRow(column(3, offset = 1,
                                 numericInput("n11", label = NULL, value = 10, min = 0, max = 100)),
                          column(3,
                                 numericInput("n12", label = NULL, value = 5, min = 0, max = 100)),
                          column(3,
                                 numericInput("n13", label = NULL, value = 0, min = 0, max = 100))
                          ),
                 fluidRow(column(3, offset = 1,
                                 numericInput("n21", label = NULL, value = 0, min = 0, max = 100)),
                          column(3,
                                 numericInput("n22", label = NULL, value = 5, min = 0, max = 100)),
                          column(3,
                                 numericInput("n23", label = NULL, value = 5, min = 0, max = 100))
                          ),
                 fluidRow(column(3, offset = 1,
                                 numericInput("n31", label = NULL, value = 0, min = 0, max = 100)),
                          column(3,
                                 numericInput("n32", label = NULL, value = 5, min = 0, max = 100)),
                          column(3,
                                 numericInput("n33", label = NULL, value = 10, min = 0, max = 100))
                          ),

                 p(strong("Set the matrix of weights")),
                 fluidRow(column(3, offset = 1,
                                 numericInput("w11", label = NULL, value = 1)),
                          column(3,
                                 numericInput("w12", label = NULL, value = -1)),
                          column(3,
                                 numericInput("w13", label = NULL, value = -99))
                          ),
                 fluidRow(column(3, offset = 1,
                      numericInput("w21", label = NULL, value = 1)),
                      column(3,
                             numericInput("w22", label = NULL, value = -1)),
                      column(3,
                             numericInput("w23", label = NULL, value = -1))
                      ),
                 fluidRow(column(3, offset = 1,
                                 numericInput("w31", label = NULL, value = 1)),
                          column(3,
                                 numericInput("w32", label = NULL, value = 1)),
                          column(3,
                                 numericInput("w33", label = NULL, value = 1))
                          ),

      






















      fluidRow(column(3, offset = 9, actionButton("reset","Reset")))
      ))),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("All Genes", br(),
                 fluidRow(
                   column(6,
                          h4(textOutput("nselect1")),
                          h4(textOutput("nunselect1"))),
                   column(6, h5("Minimum counts per cell"),
                          tableOutput("matrix"))),
                 fluidRow(
                   column(4, downloadButton("downloadAll", "Download table"), 
                          br(),
                          column(3, br(), p("Show:")),
                          column(6, selectInput("Nall", label = "", 
                                                choices = c("10", "25", "50"),
                                                selected = "10", width = "60%")),
                          br(),
                          tableOutput("alltable")),
                   column(6, 
                          column(8, h4("Some Scatterplots:")),
                          column(4, downloadButton('pdflinkAll', "Download plots")),
                          br(),
                          imageOutput("allplots"))
                   )
                 ),
        
        tabPanel("L-shaped Genes", br(),
                 h4(textOutput("nselect2")),
                 fluidRow(
                   column(4, downloadButton("downloadSelect", "Download table"), 
                          br(),
                          column(3, br(), p("Show:")),
                          column(6, selectInput("Nsel", label = "", 
                                                choices = c("10", "25", "50"),
                                                selected = "10", width = "60%")),
                          br(),
                          tableOutput("selectable")),
                   column(6, 
                          column(8, h4("Some Scatterplots:")),
                          column(4, downloadButton('pdflinkSelect', "Download plots")),
                          br(), imageOutput("selectplots"))
                   )
                 ),
        
        tabPanel("Non L-shaped Genes", br(),
                 h4(textOutput("nunselect2")),
                 fluidRow(
                   column(4, downloadButton("downloadUnsel", "Download table"), 
                          br(),
                          column(3, br(), p("Show:")),
                          column(6, selectInput("Nunsel", label = "", 
                                                choices = c("10", "25", "50"),
                                                selected = "10", width = "60%")),
                          br(),
                          tableOutput("unseltable")),
                   column(6, 
                          column(8, h4("Some Scatterplots:")),
                          column(4, downloadButton('pdflinkUnsel', "Download plots")),
                          br(), imageOutput("unselplots"))
                   )
                 )
      )
        )
      )
    ),

  tabPanel("Help",
           includeMarkdown("information/Lheuristic-info.md"))
  )
)
