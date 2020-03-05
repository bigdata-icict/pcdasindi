#' Get health units data from CNES
#'
#' This function retrieves health units data from PCDaS ElasticSearch cluster for a specified year.
#'
#' This function uses raw data from the Cadastro Nacional de Estabelecimentos de Saúde Sistema de Informações de Internações Hospitalares (SIH) available at the PCDaS ElasticSearch cluster. A documentation about this data can be found at \url{https://bigdata-metadados.icict.fiocruz.br/dataset/sistema-de-informacoes-de-mortalidade-sim}.
#' The CID-10 codes and name for chapters, group, category and subcategories of basic cause of death can be inspected at the same address above.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano_competen numeric. Competence year.
#' @param mes_competen numeric. Competence month.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param vinc_sus numeric. Health units with SUS relationship. 1 for yes and 0 for no.
#' @param leitos_hosp numeric. Health units with hospital, circury and other kinds of beds. 1 for yes and 0 for no.
#' @param tipo_unidade string. Health unit type.
#' @param esfera_admin string. Health unit administration level.
#' @param lucene_query string. A more complex lucene query can be presented here. The string must be informed using sigle quotes. If used, all other filters will be ignored.

#'
#' @return A \code{data.frame} containing number of health units for the aggregation level.
#' @examples
#' cnes <- get_cnes(conn = conn, agr = "mun", ano_competen = 2010, mes_competen = 6)
#' cnes <- get_cnes(conn = conn, agr = "mun", ano_competen = 2010, mes_competen = 6, vinc_sus = 1)
#' cnes <- get_cnes(conn = conn, agr = "mun", ano_competen = 2010, mes_competen = 6, leitos_hosp = 1)
#' cnes <- get_cnes(conn = conn, agr = "mun", ano_competen = 2010, mes_competen = 6, tipo_unidade = "HOSPITAL GERAL")

get_cnes <- function(conn, agr,
                     ano_competen = NULL,
                     mes_competen = NULL,
                     vinc_sus = NULL,
                     leitos_hosp = NULL,
                     tipo_unidade = NULL,
                     esfera_admin = NULL,
                     lucene_query = NULL){

  # Queries

  q_ano <- paste0("ano_competen:", ano_competen)
  q_mes <- paste0(" AND " ,"mes_competen:", mes_competen)

  q_vinc_sus <- NULL
  if(!is.null(vinc_sus)){
    q_vinc_sus <- paste0(" AND " ,"VINC_SUS:", vinc_sus)
  }

  q_leitos_hosp <- NULL
  if(!is.null(leitos_hosp)){
    q_leitos_hosp <- paste0(" AND " ,"LEITHOSP:", leitos_hosp)
  }

  q_tipo_unidade <- NULL
  if(!is.null(tipo_unidade)){
    q_tipo_unidade <- paste0(" AND " ,"def_tp_unid:", tipo_unidade)
  }

  q_esfera_admin <- NULL
  if(!is.null(esfera_admin)){
    q_esfera_admin <- paste0(" AND " ,"def_esfera_a:", esfera_admin)
  }


  q_lucene_query <- NULL
  if(!is.null(lucene_query)){
    q_lucene_query <- lucene_query
  }


  if(is.null(q_lucene_query)){
    query <- paste0(q_ano, q_mes, q_vinc_sus, q_leitos_hosp, q_tipo_unidade, q_esfera_admin)
  } else {
    query <- q_lucene_query
  }


  if(agr == "mun"){
    q_body <- agg_cnes_mun
    df_names <- c("cod_mun", "cnes")
  } else if (agr == "uf"){
    q_body <- agg_cnes_uf
    df_names <- c("uf", "cnes")
  } else if (agr == "regsaude"){
    q_body <- agg_cnes_regsaude
    df_names <- c("cod_reg_saude", "cnes")
  }


  # Request from ElasticSearch

  sim <- elastic::Search(conn, index="datasus-cnes-dss",
                    body = q_body,
                    q = query,
                    asdf = TRUE)

  sim <- sim$aggregations$a1$buckets
  if(is.null(nrow(sim))){ # No results
    sim <- data.frame(cod_mun = as.character(), sim = as.double()) # Empty data frame
    sim$cod_mun <- as.character()
  } else {
    names(sim) <- df_names
  }

  return(sim)
}
