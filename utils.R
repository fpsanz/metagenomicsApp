conectarBBDD <- function(usuario){
  con <- DBI::dbConnect(SQLite(), dbname = "./ddbb/usuarios.db")
  resdf <- dbGetQuery(con, paste0("Select * from usuarios where usuario ='",usuario,"';") )
  dbDisconnect(con)
  return(resdf)
}

jscode <- "shinyjs.refresh = function() { history.go(0); }"


#columnas <- input$variables -> es un vector con las variables seleccionadas
#contrast <- paste0(columnas, collapse = "_") -> nombre de la columna
#exper %>% tidyr::unite(col = !!contrast, columnas, sep="_", remove=F)
