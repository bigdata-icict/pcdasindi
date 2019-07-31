pcdas_connect <- function(host = "dados-pcdas.icict.fiocruz.br",
                          port = 443,
                          transport_schema = "https",
                          user, pwd){
  conn <- elastic::connect(
    host = host,
    port = port,
    transport_schema  = transport_schema,
    user = user,
    pwd = pwd
  )

  return(conn)
}
