#' Creates a connection to the PCDaS ElasticSearch cluster
#'
#' This function creates a connection to the ElasticSearch server from the Plataforma de Ciência de Dados aplicada à Saúde (PCDaS).
#' A username and password from PCDaS are needed in order to use this package and can be freely obtained at \url{https://bigdata.icict.fiocruz.br}.
#' @param host ElasticSearch cluster address from PCDaS.
#' @param port Port number.
#' @param transport_schema Only 'https' connection is supported.
#' @param user Username at PCDaS.
#' @param pwd_mode "dialog" will use an interactive dialog asking for the password. Use "text" to inform the password with 'pwd_txt' (usefull case for automations).
#' @param pwd_txt Password in plain text.
#' @return A connection instruction to the PCDaS ElasticSearch cluster.
#' @examples
#' pcdas_connect(user = "your_username")
#' pcdas_connect(user = "your_username", pwd_mode = "text", pwd_txt = "your_password")

pcdas_connect <- function(host = "dados-pcdas.icict.fiocruz.br",
                          port = 443,
                          transport_schema = "https",
                          user, pwd_mode = "dialog", pwd_txt){

  if(pwd_mode == "dialog"){
    pwd <- getPass::getPass(msg = "PCDaS password")
  } else if(pwd_mode == "text"){
    pwd <- pwd_txt
  }


  conn <- elastic::connect(
    host = host,
    port = port,
    transport_schema  = transport_schema,
    user = user,
    pwd = pwd
  )

  return(conn)
}
