rootdir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

lib_path <- file.path(rootdir, "census_1928")

setwd(lib_path)

source('libraries.R')

setwd(path)

map<-st_read('mun.shp')

map<-ms_simplify(map)

data<-read.xlsx('mun.xlsx')

options(mapbox.accessToken = 'pk.eyJ1IjoibWNoYXR6aWdha2lzIiwiYSI6ImNsZW4wempkajFhdzAzdnBnbjFpMnl2dHQifQ.LxP677Ldt2WHej2Wmf1kQA')

header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(  
  tags$style(type = "text/css"),
  tags$style("#menu {'/*background-color:black;*/
                     height: calc(100vh - 12px) !important;
                     margin-top: -50px;
                    }
                     
                     .reset-button {
                      background-color: rgb(0,0,0,0) !important;
                      color: white;
                      border: none;
                      padding: 0;
                      cursor: pointer;
                      transition: background-color 0s;
                      position: relative;
                    }
                    
                    .reset-button::after {
                      content: 'Reset Filters';
                      position: absolute;
                      bottom: -30px;
                      left: 250%;
                      transform: translateX(-50%);
                      color: white;
                      background-color: rgba(0, 0, 0, 0);
                      padding: 5px;
                      border-radius: 5px;
                      font-weight: bold;
                      opacity: 0;
                      visibility: hidden;
                      transition: opacity 0.3s, visibility 0.3s;
                    }
                    
                    .reset-button:hover::after {
                      opacity: 1;
                      visibility: visible;
                    }
             
                    .fa-download:hover{
                      color:white !important;
                    }
      
                    .fa-envelope:hover{
                       color:white !important;
                    }
                    
                    .fa-github:hover{
                       color:white !important;
                    }
                    
                    .fa-twitter:hover{
                       color:white !important;
                    }
                    
                    .fa-linkedin:hover{
                       color:white !important;
                    }
                    
                    .fa-facebook:hover{
                       color:white !important;
                    }
                    .fa-square-facebook:hover{
                       color:white !important;
                    }"),
  width = 275,
  useShinyjs(),
  div(id = "menu",
      br(),
      div(style = 'margin-left:15px',
          span(style = 'color:white; font-weight: bold; font-size:20px', 'Απογραφή 1928')),
      br(),
      div(style = 'margin-left:15px',
          span(style = 'color:white; font-weight: bold; font-size:14px', 'Πληθυσμός: 6,210,310')),
      br(),
      div(style = 'margin-left:30px',
          span(style = 'color:white; font-weight: bold; font-size:14px', HTML(paste0('&#9654;&nbsp;&nbsp;Γηγενείς: 5,058,282')))),
      br(),
      div(style = 'margin-left:30px',
          span(style = 'color:white; font-weight: bold; font-size:14px', HTML(paste0('&#9654;&nbsp;&nbsp;Πρόσφυγες: 1,152,028')))),
      br(),
      selectInput(inputId = "input_variable", 
                  label = tags$span(style="color:white; font-weight: bold","Μεταβλητή"), 
                  choices = c("Πληθυσμός", "Πληθυσμός Προσφύγων", "Ποσοστό Προσφύγων"), 
                  selected ="Πληθυσμός",
                  selectize = FALSE),
      selectInput(inputId = "input_gender", 
                  label = tags$span(style = "color:white; font-weight: bold","Φύλο"), 
                  choices = c("Σύνολο", "Άνδρες", "Γυναίκες"), 
                  selected ="Σύνολο",
                  selectize = FALSE),
      div(
        style = 'margin-left: 1px;',
        actionButton("reset_button",
                     label = "",
                     icon = icon("repeat"),
                     class = "reset-button"
        )),
      div(style = "margin-left: 15px;", HTML("<a href=\"census1928_mun.zip\" <i class='fa-solid fa-download' style='font-size: 1.5em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Download map data'></i> </a>
                                             <a href=\"mailto:mchatzigakis@london.edu\" <i class='fa-solid fa-envelope' style='font-size: 1.5em; margin-left: 2em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Give us feedback'></i> </a>
                                             <a href=\"https://github.com/MChatzigakis/census-1928\" target='_blank'> <i class='fa-brands fa-github' style='font-size: 1.5em; margin-left: 4em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Check out the source code'></i> </a>
                                             <a href=\"#\" onclick=\"window.open('https://www.twitter.com/intent/tweet?text=Map%20the%20census%20of%201928&url=https%3A%2F%2Fanatolia.int.webjar.gr%2F', 'popup', 'width=600,height=600')\"> <i class='fa-brands fa-twitter' style='font-size: 1.5em; margin-left: 6em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Tweet about the map'></i> </a>
                                             <a href=\"#\" onclick=\"window.open('https://www.linkedin.com/uas/login?session_redirect=https%3A%2F%2Fwww.linkedin.com%2FshareArticle%3Fmini%3Dtrue%26url%3Dhttps%253A%252F%252Fanatolia.int.webjar.gr%252F%26titleCensus%2520of%25201928%2520Atlas%26summary%3DMap%2520the%2520census%2520of%2520of%25201928','name','width=600,height=600')\"> <i class='fa-brands fa-linkedin' style='font-size: 1.5em; margin-left: 8em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Share on LinkedIn'></i> </a>
                                             <a href=\"#\" onclick=\"window.open('https://www.facebook.com/sharer/sharer.php?u=https://anatolia.int.webjar.gr&quote=Map%20the%20census%20of%201928', '_blank', 'width=600,height=600')\"> <i class='fa-brands fa-square-facebook' style='font-size: 1.5em; margin-left: 10em; margin-bottom: 0.5em; position: absolute; bottom: 0;' title='Share on Facebook'></i> </a>"))
  )
)

body <- dashboardBody(useShinyjs(),
                      leafletOutput("map"),
                      tags$style(type = "text/css", 
                                 "#map {height: calc(100vh - 0.5px) !important;
                                               width: calc(100vw - 276px) !important;
                                               margin-top: -30px;
                                               top: 15px;          
                                               left: -14.8px !important;}"))

ui <- tagList(
  tags$style("html,body{background-color: black;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
                "), tags$div(class="container",dashboardPage(header, sidebar, body)))