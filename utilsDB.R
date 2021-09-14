usuariosCredenciales <- function(user){
  conn <- DBI::dbConnect(SQLite(), dbname = "./ddbb/credenciales.sqlite")
  df <- read_db_decrypt(conn, name = "credentials", passphrase = "fps379725")
  dbDisconnect(conn)
  return(df$user)
}

recuperarDatosUsuario <- function(usuario){
  conn <- DBI::dbConnect(SQLite(), dbname = "./ddbb/usuarios.db")
  identif = usuario
  resdf <- dbGetQuery(conn, "Select * from usuarios where usuario=?", params=identif )
  dbDisconnect(conn)
  return(resdf)
}

insertarFicheroBD <- function(usuario,mail, fichero){
  conn <- DBI::dbConnect(SQLite(), dbname = "./ddbb/usuarios.db")
  usuario = usuario
  fichero = fichero
  mail = mail
  id = paste0(usuario,mail,fichero)
  getId <- dbGetQuery(conn, "select id from usuarios where id = ?", list(id))
  if(length(getId$id)==0){
    dbExecute(conn, "insert into usuarios (id, usuario, mail, fichero) values (?, ?, ?, ?) ",
              list(id, usuario,mail,fichero) )
    dbDisconnect(conn)
    return(TRUE)
  }else{
    dbDisconnect(conn)
    return(FALSE)
  }
}

borrarRegistrosBD <- function(ids){
  conn <- DBI::dbConnect(SQLite(), dbname = "./ddbb/usuarios.db")
  dbExecute(conn, "DELETE FROM usuarios WHERE id IN (?);", list(ids))
  dbDisconnect(conn)
  return(TRUE)
}



jscode <- "shinyjs.refresh = function() { history.go(0); }"