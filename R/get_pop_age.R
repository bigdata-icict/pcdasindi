#' Get population from SVS with sex and age
#'
#' This function retrieves population data from PCDaS ElasticSearch cluster for a specified year.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @return A \code{data.frame} containing population for the aggregation level.
#' @examples
#' pop_df <- get_pop_mun(conn = conn, ano = 2010, agr = "mun")

get_pop_age <- function(conn, ano, agr){

  query <- paste0("ANO:", ano)

  if (agr == "mun") {
    q_body <- agg_pop_mun_age
    df_names <- "cod_mun"
  }
  else if (agr == "uf") {
    q_body <- agg_pop_uf_age
    df_names <- "uf"
  }
  else if (agr == "regsaude") {
    q_body <- agg_pop_regsaude_age
    df_names <- "cod_reg_saude"
  }

  pop <- elastic::Search(conn, index = "svs-pop-dss",
                             body = q_body, q = query, asdf = TRUE)

  nomes <- c(df_names, "ate04","de5a9","de10a14","de15a19","de20a24","de25a29","de30a34","de35a39","de40a44","de45a49","de50a54","de55a59","de60a64","de65a69","de70a74","de75a79","de80")

  # Feminino

  fem <- pop[["aggregations"]][["3"]][["buckets"]][["4.buckets"]][[1]][["2.buckets"]]
  df_fem <- dplyr::bind_cols(fem) %>%
    dplyr::select(key, dplyr::matches("value")) %>%
    dplyr::mutate(sexo = "Feminino")

  names(df_fem) <- c(nomes,"sexo")

  df_fem <- df_fem %>% tidyr::gather(faixa, pop, -df_names, -sexo)


  # Masculino

  masc <- pop[["aggregations"]][["3"]][["buckets"]][["4.buckets"]][[2]][["2.buckets"]]
  df_masc <- dplyr::bind_cols(masc) %>%
    dplyr::select(key, dplyr::matches("value")) %>%
    dplyr::mutate(sexo = "Masculino")

  names(df_masc) <- c(nomes,"sexo")

  df_masc <- df_masc %>% tidyr::gather(faixa, pop, -df_names, -sexo)


  # Juntos

  df <- dplyr::bind_rows(list(df_fem, df_masc))

  return(df)

}
