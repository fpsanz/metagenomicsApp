tabPanel(value = "abundanceplot", title = "Abundance plots",
 fluidRow(
   column(width = 8, offset = 2,
          bsAlert("messageBoxplot"),
          h3("Boxplot top 25 more abuncance taxa"),
          plotOutput("boxplot", height = "800px")
          )
 ),
 br(),
 fluidRow(
   column(width = 8, offset = 2,
          #h3("Boxplot top 25 more abuncance taxa"),
          h3("Abundance barplot by groups"),
          plotOutput("barplot1", height = "800px")
   )
 ),
 br(),
 fluidRow(
   column(width = 2,
          uiOutput("selectCondition"),
          uiOutput("selectSample")
          ),
   column(width = 8,
          #h3("Boxplot top 25 more abuncance taxa"),
          h3("Abundance barplot by sample"),
          plotOutput("barplot2", height = "800px")
   )
 )
)