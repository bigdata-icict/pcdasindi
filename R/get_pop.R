#' Get population from SVS
#'
#' This function retrieves population data from PCDaS ElasticSearch cluster for a specified year.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @return A \code{data.frame} containing population for the aggregation level.
#' @examples
#' pop_df <- get_pop_mun(conn = conn, ano = 2010, agr = "mun")

get_pop <- function(conn, ano, agr){

  query <- paste0("ANO:", ano)

  if(agr == "mun"){
    q_body <- agg_pop_mun
    df_names <- c("cod_mun", "pop")
  } else if (agr == "uf"){
    q_body <- agg_pop_uf
    df_names <- c("uf", "pop")
  } else if (agr == "regsaude"){
    q_body <- agg_pop_regsaude
    df_names <- c("cod_reg_saude", "pop")
  }

  pop <- elastic::Search(conn, index="svs-pop-dss",
                    body = q_body,
                    q = query,
                    asdf = TRUE)

  pop <- pop$aggregations$a1$buckets
  pop$doc_count <- NULL
  names(pop) <- df_names

  return(pop)

}
