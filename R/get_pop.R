#' Get population
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

  pop <- try(elastic::Search(conn, index="svs-pop-dss",
                    body = q_body,
                    q = query,
                    asdf = TRUE))
  if(class(pop) == "try-error") {
    message("It was not posible to connect to the PCDaS ElasticSearch cluster.")
    message("Your username or password may be incorrect or the cluster is offline.")
    message("Please check your credentials or try again later.")
  } else {
    pop <- pop$aggregations$a1$buckets
    pop$doc_count <- NULL
    names(pop) <- df_names

    return(pop)
  }
}
