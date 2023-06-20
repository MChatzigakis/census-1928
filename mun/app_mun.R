rm(list=ls())
cat("\014")

rootdir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

path <- file.path(rootdir, "census_1928", "mun")

setwd(path)

ui <- source('ui_mun.R')

server <- source('server_mun.R')

runApp(shinyApp(ui = ui$value, server = server$value), launch.browser = TRUE)
