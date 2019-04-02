
fluidRow(
  column(width = 4,
         wellPanel(
           
           h3("Select L-shape parameters:", align = "left"),
           numericInput("Ngenes",label = "Number of genes to analyse", value = 200, min = 0),
           checkboxInput("allgenes", label="Analyse all genes", value=FALSE),
           
           actionButton("submit","Select genes", icon("check"),
                                                       style="color: #fff; background-color: #CD0000; border-color: #9E0000"),
      
           br(),
           br(),
           
           sliderInput("xaxis", "Coordinates of vertical points in the x-axis",
                       min = 0, max = 1, value = c(1/3, 2/3)),
           sliderInput("yaxis", "Coordinates of vertical points in the y-axis",
                       min = 0, max = 1, value = c(1/3, 2/3)),
           
           p(strong("Min/Max counts per cell (%)")),
           fluidRow(column(3, offset = 1,
                           numericInput("n11", label = NULL, value = 10, min = 0, max = 100)),
                    column(3,
                           numericInput("n12", label = NULL, value = 20, min = 0, max = 100)),
                    column(3,
                           numericInput("n13", label = NULL, value = 1, min = 0, max = 100))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("n21", label = NULL, value = 5, min = 0, max = 100)),
                    column(3,
                           numericInput("n22", label = NULL, value = 40, min = 0, max = 100)),
                    column(3,
                           numericInput("n23", label = NULL, value = 20, min = 0, max = 100))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("n31", label = NULL, value = 0, min = 0, max = 100)),
                    column(3,
                           numericInput("n32", label = NULL, value = 5, min = 0, max = 100)),
                    column(3,
                           numericInput("n33", label = NULL, value = 10, min = 0, max = 100))
           ),
           
           p(strong("Set the matrix of weights for TRUE L scatterplots")),
           fluidRow(column(3, offset = 1,
                           numericInput("w11", label = NULL, value = 2)),
                    column(3,
                           numericInput("w12", label = NULL, value = -2)),
                    column(3,
                           numericInput("w13", label = NULL, value = -25))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("w21", label = NULL, value = 1)),
                    column(3,
                           numericInput("w22", label = NULL, value = 0)),
                    column(3,
                           numericInput("w23", label = NULL, value = -2))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("w31", label = NULL, value = 1)),
                    column(3,
                           numericInput("w32", label = NULL, value = 1)),
                    column(3,
                           numericInput("w33", label = NULL, value = 2))
           ),
           
           p(strong("Set the matrix of weights for FALSE L scatterplots")),
           fluidRow(column(3, offset = 1,
                           numericInput("wf11", label = NULL, value = 0)),
                    column(3,
                           numericInput("wf12", label = NULL, value = -2)),
                    column(3,
                           numericInput("wf13", label = NULL, value = -25))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("wf21", label = NULL, value = 0)),
                    column(3,
                           numericInput("wf22", label = NULL, value = 0)),
                    column(3,
                           numericInput("wf23", label = NULL, value = -2))
           ),
           fluidRow(column(3, offset = 1,
                           numericInput("wf31", label = NULL, value = 0)),
                    column(3,
                           numericInput("wf32", label = NULL, value = 0)),
                    column(3,
                           numericInput("wf33", label = NULL, value = 0))
           ),
           actionButton("reset","Reset")
  )),

column(width = 8,
       
  fluidPage(tabsetPanel(
    
    tabPanel("All Genes", br(),
             fluidRow(
               column(6,
                      h4(textOutput("nselect1")),
                      h4(textOutput("nunselect1"))),
               column(6, h5("Min/Max counts per cell"),
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
  )))
)

