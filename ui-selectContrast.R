tabPanel(value = "contrast", title = "Contrast Preview",
      fluidRow(
        column(width = 2,
               uiOutput("contrastSel") ),
        column(width=8, offset = 3,
               bsAlert("messageVariable"),
               DT::dataTableOutput("tablasRes")
        )
      )  
)