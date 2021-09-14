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
library("RSQLite")
library(DT)
library(shinymanager)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
source("utils.R")


ui <- fluidPage(
    fluidRow(class= "headerLogo", 
             column(width = 3,
                 HTML('<a href="http://www.imib.es/web/personal.jsf?id=7961" target="_blank"><img src="imibNombre.png" alt="imib", style="height:40px; padding-top:3px;"></a>')
             )),
    navbarPage(id ="navpanel",
               title ="Metagenomics viewer",
               theme = shinytheme("superhero"),
               collapsible = TRUE,
               fluid = TRUE,
               source(file = "ui-home.R", local = TRUE, encoding = "UTF-8")$value,
               source(file = "ui-selectContrast.R", local = TRUE, encoding = "UTF-8")$value,
               #source(file = "ui-corrplot.R", local = TRUE, encoding = "UTF-8")$value,
               #source(file = "ui-contributions.R", local = TRUE, encoding = "UTF-8")$value,
               #source(file = "ui-hierarplot.R", local = TRUE, encoding = "UTF-8")$value,
               #source(file = "ui-hcpc.R", local = TRUE, encoding = "UTF-8")$value,
               #source(file = "ui-help.R", local = TRUE, encoding = "UTF-8")$value,
               includeCSS("./www/mystyle.css"),
               setShadow(class = "box"),
               useShinyalert()
    )
    
)


ui <- secure_app(
        ui,
        enable_admin = TRUE,
        theme = shinythemes::shinytheme("darkly"),
        head_auth = HTML(
            "<style>
                 .panel-auth{background-color: #343e48 !important;}
            </style>"),
        tags_bottom = tagList(tags$div(style = "text-align: center;",))
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #comprobar credenciales
    auth <- secure_server(check_credentials = 
                              check_credentials(
                                  "./ddbb/credenciales.sqlite",
                                  passphrase = readRDS("./ddbb/dbpass.Rds"))
                          )
    
    #definir variable que contendrá al usuario
    user <- reactive({user <- auth})
    #definir variable que contiene el fichero seleccionado
    fichero <- reactiveValues(file=NULL)
    matrizDatos <- reactiveValues() #contenedor de datos
    
    # modifica contenido de variable fichero según opcion seleccionada
    observeEvent(input$ficheros, {
        fichero$file <- input$ficheros
    })
    
    #renderiza un selector de ficheros en base al usuario
    output$files <- renderUI({
        validate(need(user()$user,""))
        usuario <- user()$user
        respuesta <- conectarBBDD(usuario)
        if(length(respuesta$usuario)==0){
            shinyalert::shinyalert("Actualmente no existe usuario con ficheros",
                                   type = "error",
                                   callbackJS = jscode)
        }else{
            selectInput("ficheros", 
                        label = "Select your file",
                        choices = respuesta$fichero, selected = NULL, multiple = FALSE )
        }
    })
    datos <- reactiveValues()
    ## cargar fichero
    observeEvent(input$send,{
        objetos <- load(file = paste0("data/",input$ficheros), envir = .GlobalEnv)
        datos$exper <- get(objetos[1], .GlobalEnv)
        datos$otu <- get(objetos[2], .GlobalEnv)
    })
    ## Mostrar preview de datos
    output$matrix <- DT::renderDataTable({
        if(is.null(datos$otu) ){
            createAlert(session, "datosmessage", alertId ="messagedatos", 
                        title = "Missing data", content = "Please select file to preview data", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagedatos")
            datos$otu %>% head(100) %>% DT::datatable( 
                          style = "bootstrap4", options = list(scrollX=TRUE))
        }            
    })
    ## Mostrar coldata del experimento
    output$sampleData <- DT::renderDataTable({
        if(is.null(datos$exper) ){
            createAlert(session, "samplesmessage", alertId ="messagesamples", 
                        title = "Missing samples info",
                        content = "Please select file to preview data about samples", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagesamples")
            datos$exper %>% DT::datatable(
                          style = "bootstrap4", options = list(scrollX=TRUE))
        }
    })
    ##renderizar selector de variables para el contraste
    output$variables <- renderUI({
        if(is.null(datos$exper)){
            createAlert(session, "messageVariable", "variablemessage",
                       title = "Missing samples info",
                       content = "Please select file to select variable(s)",
                       append = FALSE, style ="danger")
            return(NULL)
        }else{
            closeAlert(session, "variablemessage")
            selectInput("selectvariable", label = "Select variable(s) to contrast",
                        choices = colnames(datos$exper), multiple = TRUE, selected = NULL)
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
