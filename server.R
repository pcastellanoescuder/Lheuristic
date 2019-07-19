#---------------------------------------------------------------
# Versio 1.1 Mars-2018
#---------------------------------------------------------------

# We will use the set of functions in gridFunctions.R
# source("gridFunctions.R")
library(Lheuristic)

#Set working directory
workingDir <- getwd()
dataDir <- file.path(workingDir, "data")

# Set memory size limit to 30 Mb. Intended to allow uploading big files

options(shiny.maxRequestSize=30*1024^2)

#server
shinyServer(function(input, output, clientData, session) {
  
  source("server-upload.R",local = TRUE)
  source("server-correlations.R",local = TRUE)
  source("server-lheuristic.R",local = TRUE)
  source("server-overlap.R",local = TRUE)

})

