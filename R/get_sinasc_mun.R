get_sinasc_mun <- function(conn, ano){

  #source("R/agg_sinasc_mun.R", local = TRUE)

  query <- paste0("ano_nasc:", ano)

  sinasc <- elastic::Search(conn, index="datasus-sinasc-dss",
                body = agg_sinasc_mun,
                q = query,
                asdf = TRUE)

  sinasc <- sinasc$aggregations$a1$buckets
  names(sinasc) <- c("cod_mun", "sinasc")

  return(sinasc)

}
