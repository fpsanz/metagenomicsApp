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
library(shinyalert)
library(shinymanager)
source("utilsDB.R")


### HEADER ##############
header <- dashboardHeader()

### SIDEBAR ##############
sidebar <- dashboardSidebar()

### BODY ##############
body <- dashboardBody(
    useShinyalert(),
    shinyjs::useShinyjs(),
    fluidRow(column(width = 11, offset=1, uiOutput("users"))),
    fluidRow(
        column(width = 2, offset = 1,
            uiOutput("mail")),
        column(width = 2,
            textInput("fichero","Añadir fichero"))),
    fluidRow(
        column(width = 1, offset = 1,
               actionButton("validarFicheroNuevo", "Añadir")
               )
    ),
    fluidRow(column(width = 6, offset = 3,
        box(align = 'center',
            title = "Cutoff values",
            solidHeader = FALSE,
            status = "primary",
            width = NULL,
        DTOutput("tablaUsuario"),
        actionButton("remove", "Click confirm remove")
        )
    ))
)

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
#### SERVER ###################
server <- function(input,output,session){
    #comprobar credenciales
    auth <- secure_server(check_credentials = check_credentials("./ddbb/credenciales.sqlite",
                                                                passphrase = readRDS("./ddbb/dbpass.Rds")),
    )
    
    #definir variable que contendrá al usuario
    user <- reactive({user <- auth})
    
    output$users <- renderUI({
        validate(need(user()$user,""))
        usuario <- user()$user
        usuariosCredenciales <- usuariosCredenciales()
        selectInput("usuarios", 
                        label = "Select your file",
                        choices = usuariosCredenciales,
                    selected = NULL, multiple = FALSE )
        
    })

    output$mail <- renderUI({
        dfUsuario$val <- recuperarDatosUsuario(input$usuarios)
        if(length(dfUsuario$val$mail)==0){
            valor = ""
            #placeholder = ""
        }else{
            valor = unique(dfUsuario$val$mail)
            #placeholder = unique(dfUsuario$val$mail)
        }
        textInput("inputMail", "Añadir mail", value=valor )
    })
    
    dfUsuario <- reactiveValues(val=NULL)
    
    #añadir registros
    observeEvent(input$validarFicheroNuevo,{
        
        if(input$inputMail == ""){
                 shinyalert("El campo mail no puede estar vacio")
         }else{
            if(input$fichero == ""){
                shinyalert("Debe introducir un nombre fichero valido")
            }else{
                dfUsuario$val <- recuperarDatosUsuario(input$usuarios)
                usuario <- input$usuarios
                if(length(dfUsuario$val$mail)!=0){
                    mail <- unique(dfUsuario$val$mail)
                }else{ 
                    mail <- input$inputMail
                    }
                fichero <- input$fichero
                resultado <- insertarFicheroBD(usuario = usuario,mail = mail,fichero = fichero)
                dfUsuario$val <- recuperarDatosUsuario(input$usuarios)
                if(!isTRUE(resultado)){
                    shinyalert("Esa combinación de usuario/mail/fichero ya existe")
                    }
                }
             }
        updateTextInput(session, "inputMail", value = NULL)
    })
    
    #renderizar tabla
    output$tablaUsuario <- renderDataTable({
        validate(need(input$usuarios,""))
        dfUsuario$val <- recuperarDatosUsuario(input$usuarios)
        DT::datatable(dfUsuario$val[,-c(1)], rownames = FALSE)
    })
    
    # eliminar registros
    observeEvent(input$remove,{
        selectedRows <- input$tablaUsuario_rows_selected
        res <- borrarRegistrosBD(dfUsuario$val[selectedRows,1])
        if(isTRUE(res)){
        dfUsuario$val <- recuperarDatosUsuario(input$usuarios)
        shinyalert("Registros Borrados", timer = 1000)
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
