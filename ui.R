options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")

source("helpers.R")

dashboardPage(
              
              dashboardHeader(title = "Selection of L-shaped genes using a heuristic algorithm", 
                              titleWidth = 600),
              
              dashboardSidebar(sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
                
                
                menuItem("Analysis", tabName = "analysis", icon = icon("sliders"), startExpanded = FALSE,
                         menuSubItem("Correlations", tabName = "correlations", icon = icon("chart-line")),
                         menuSubItem("L-heuristic", tabName = "lheuristic", icon = icon("ruler-combined"))),
                menuItem("Help", tabName = "help", icon = icon("question"))
                
              )),
              dashboardBody(
                
                shinyDashboardThemes(
                  theme = "onenote"),

                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
                ),
                tabItems(
                  tabItem(tabName = "home",
                          source("ui-tab-home.R",local=TRUE)$value),
                  tabItem(tabName = "correlations",
                          source("ui-tab-correlations.R",local=TRUE)$value),
                  tabItem(tabName = "lheuristic",
                          source("ui-tab-lheuristic.R",local=TRUE)$value),
                  tabItem(tabName = "upload",
                          source("ui-tab-upload.R",local=TRUE)$value),
                  tabItem(tabName = "help",
                          source("ui-tab-help.R",local=TRUE)$value)
                  
                ),
                
                tags$hr(),
                
                ## FOOTER
                
                tags$footer(p(HTML("<b>Statistics and Bioinformatics Research Group</b>"), align="center",width=3),
                            p(("Department of Genetics, Microbiology and Statistics"),align="center",width=3),
                            p(("University of Barcelona"),align="center",width=3),
                            p(("Copyright (C) 2019, code licensed under GPLv3"),align="center",width=4),
                            p(("Code available on Github:"),a("https://github.com/alexsanchezpla", 
                                                              href="https://github.com/alexsanchezpla"),align="center",width=4))#,
                
                ## GOOGLE ANALYTICS
                
                #tags$head(includeScript("google-analytics.js"))
              )) 

