tabPanel(value = "abundanceplot", title = "Abundance plots",
 fluidRow(
   column(width = 8, offset = 2,
          bsAlert("messageBoxplot"),
          plotOutput("boxplot")
          )
 )
)