#' Get population for municipalities
#'
#' This function retrieves population for all Brazilian municipalities from PCDaS ElasticSearch cluster for a specified year.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and population (\code{pop}).
#' @examples
#' pop_df <- get_pop_mun(conn = conn, ano = 2010)

get_pop_mun <- function(conn, ano){

  query <- paste0("ANO:", ano)

  pop <- elastic::Search(conn, index="svs-pop-dss",
                    body = agg_pop_mun,
                    q = query,
                    asdf = TRUE)

  pop <- pop$aggregations$a1$buckets
  pop$doc_count <- NULL
  names(pop) <- c("cod_mun", "pop")

  return(pop)
}
