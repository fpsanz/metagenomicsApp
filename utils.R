conectarBBDD <- function(usuario){
  con <- DBI::dbConnect(SQLite(), dbname = "usuarios.db")
  resdf <- dbGetQuery(con, paste0("Select * from usuarios where usuario ='",usuario,"';") )
  dbDisconnect(con)
  return(resdf)
}

jscode <- "shinyjs.refresh = function() { history.go(0); }"