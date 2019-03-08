options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")

source("helpers.R")
#source("themes.R")

dashboardPage(skin = "black", 
              
              dashboardHeader(title = "Selection of L-shaped genes using a heuristic algorithm"),
              
              dashboardSidebar(sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                #menuItem("Correlations", tabName = "correlations", icon = icon("wrench")),
                menuItem("L-heuristic", tabName = "lheuristic", icon = icon("sliders")),
                #menuItem("Joint analysis", tabName = "jointanalysis", icon = icon("sliders")),
                menuItem("Help", tabName = "help", icon = icon("question"))
                
              )),
              dashboardBody(
                
                #poma_theme,
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
                ),
                tabItems(
                  tabItem(tabName = "home",
                          source("ui-tab-home.R",local=TRUE)$value),
                  #tabItem(tabName = "correlations",
                         # source("ui-tab-correlations.R",local=TRUE)$value),
                  tabItem(tabName = "lheuristic",
                          source("ui-tab-lheuristic.R",local=TRUE)$value),
                  #tabItem(tabName = "jointanalysis",
                   #       source("ui-tab-jointanalysis.R",local=TRUE)$value),
                  tabItem(tabName = "help",
                          source("ui-tab-help.R",local=TRUE)$value)
                  
                ),
                
                tags$hr(),
                
                ## FOOTER
                
                tags$footer(p(HTML("<b>Statistics and Bioinformatics Research Group</b>"), align="center",width=3),
                            p(("University of Barcelona"),align="center",width=3),
                            p(("Copyright (C) 2019, code licensed under GPLv3"),align="center",width=4),
                            p(("Code available on Github:"),a("https://github.com/alexsanchezpla", 
                                                              href="https://github.com/alexsanchezpla"),align="center",width=4))#,
                
                ## GOOGLE ANALYTICS
                
                #tags$head(includeScript("google-analytics.js"))
              )) 

