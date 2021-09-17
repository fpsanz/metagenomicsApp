tabPanel("Home",
         column(
           width = 2,
           box(
             width = 12,
             height = "800px",
             title = "",
             status = "info",
             column(width = 11, offset = 1,
                    br(),
                    uiOutput("files"),
                    actionButton("send", "Load"),
                    br(),br(),
                    uiOutput("variables"),
                    uiOutput("removeSamples"),
                    br(),
                    uiOutput("variablesbtn")
             ),# fin column box
             tags$div(class = "bottomdiv",
                      column(width = 9, offset = 1,
                             tags$hr(),
                             fluidRow(
                               actionButton("aboutButton", "About"),
                               actionButton(inputId = "resetbutton", "Reset", size = "md", color = "danger"))
                      ),
                      style="position: absolute; bottom: 10px; width:100%;")
           )
         ),
         column(width = 10,
                fluidRow(
                  column(width = 8, offset = 2,
                         box( width = 12, status = "info",
                              bsAlert("datosmessage"),
                              DT::dataTableOutput("matrix")
                         )
                  )),
                br(),
                fluidRow(
                  column(width = 8, offset = 2,
                         box( width = 12 ,status = "info",
                              bsAlert("samplesmessage"),
                              DT::dataTableOutput("sampleData")
                         ) 
                  )
                )
                )#fin column
  
)