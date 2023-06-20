rm(list=ls())
cat("\014")
  
rootdir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
  
path <- file.path(rootdir, "census_1928", "prov")

setwd(path)
  
ui <- source('ui_prov.R')
  
server <- source('server_prov.R')
  
runApp(shinyApp(ui = ui$value, server = server$value), launch.browser = TRUE)
