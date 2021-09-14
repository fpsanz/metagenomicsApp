tabPanel(value = "contrast", title = "Contrast Selection",
      fluidRow(
        column(width=4, offset = 4,
               bsAlert("messageVariable")#,
               #uiOutput("variables")
        )
      )  
)