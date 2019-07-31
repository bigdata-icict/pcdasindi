#' Get live births data for municipalities
#'
#' This function retrieves live births data for all Brazilian municipalities from PCDaS ElasticSearch cluster for a specified year.
#'
#' This function uses raw data from the Sistema de Informações de Nascidos Vivos (SINASC) available at the PCDaS ElasticSearch cluster. A documentation about this data can be found at \url{https://bigdata-metadados.icict.fiocruz.br/dataset/sistema-de-informacoes-de-nascidos-vivos-sinasc}.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year of birth.
#'
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and number of live births (\code{sinasc}).
#' @examples
#' sinasc <- get_sinasc_mun(conn = conn, ano = 2010)

get_sinasc_mun <- function(conn, ano){

  query <- paste0("ano_nasc:", ano)

  sinasc <- elastic::Search(conn, index="datasus-sinasc-dss",
                body = agg_sinasc_mun,
                q = query,
                asdf = TRUE)

  sinasc <- sinasc$aggregations$a1$buckets
  names(sinasc) <- c("cod_mun", "sinasc")

  return(sinasc)

}
