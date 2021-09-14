#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RSQLite)
library(tidyverse)
library(shinydashboard)

header <- dashboardHeader()
sidebar <- dashboardSidebar()
body <- dashboardBody()

ui <- dashboardPage(title = "Database update",
                    header,
                    sidebar,
                    body)

ui <- secure_app(ui, enable_admin = TRUE, theme = shinythemes::shinytheme("darkly"),
                 head_auth = HTML("<style>
                 .panel-auth{
                                  background-color: #343e48 !important;
                                  }
                                  </style>"
                                  ),
                 tags_bottom = tagList(tags$div(style = "text-align: center;",
                   
                   )
                  )
                 )

server <- function(input,output,session){
    #comprobar credenciales
    auth <- secure_server(check_credentials = check_credentials("../credenciales.sqlite",
                                                                passphrase = readRDS("../dbpass.Rds")),
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
