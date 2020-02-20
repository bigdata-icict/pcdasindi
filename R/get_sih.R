#' Get mortality data from SIH
#'
#' This function retrieves hospital admission data from PCDaS ElasticSearch cluster for a specified year.
#'
#' This function uses raw data from the Sistema de Informações de Internações Hospitalares (SIH) available at the PCDaS ElasticSearch cluster. A documentation about this data can be found at \url{https://bigdata-metadados.icict.fiocruz.br/dataset/sistema-de-informacoes-de-mortalidade-sim}.
#' The CID-10 codes and name for chapters, group, category and subcategories of basic cause of death can be inspected at the same address above.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year of death.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param sexo string. Sex of the patient \code{Masculino} for males or \code{Feminino} for females.
#' @param diag_princ string. CID-10 code for the main diagnosis.
#' @param diag_princ_capitulo string. Chapter of the main diagnosis.
#' @param diag_princ_grupo string. Group of the main diagnosis.
#' @param diag_princ_categoria string. Category of the main diagnosis.
#' @param diag_princ_subcategoria string. Subcategory of the main diagnosis.
#' @param idade_anos_min numeric. Minimum age of patient, in years.
#' @param idade_anos_max numeric. Maximum age of patient, in years.
#' @param proced_real numeric. Numeric code of the realized hospital procedure.
#' @param lucene_query string. A more complex lucene query can be presented here. The string must be informed using sigle quotes. If used, all other filters will be ignored.

#'
#' @return A \code{data.frame} containing number of hospital admission authorizations (\code{sih}) for the aggregation level.
#' @examples
#' sim <- get_sih(conn = conn, ano = 2010, agr = "mun", sexo = "Masculino")
#' sim <- get_sih(conn = conn, ano = 2010, agr = "mun", diag_princ = "J448")
#' sim <- get_sih(conn = conn, ano = 2010, agr = "mun", idade_anos_min = 0, idade_anos_max = 10)
#' sim <- get_sih(conn = conn, ano = 2010, agr = "mun", proced_real = 310010039)
#' sih <- get_sih(conn = conn, agr = "mun", lucene_query = 'ano_internacao: 2010 AND def_raca_cor: "Preta" or "Branca"')

get_sih <- function(conn, agr,
                        ano = NULL,
                        sexo = NULL,
                        diag_princ = NULL,
                        diag_princ_capitulo = NULL,
                        diag_princ_grupo = NULL,
                        diag_princ_categoria = NULL,
                        diag_princ_subcategoria = NULL,
                        idade_anos_min = NULL, idade_anos_max = NULL,
                        proced_real = NULL,
                        lucene_query = NULL){

  # Queries

  q_year <- paste0("ano_internacao:", ano)

  q_sexo <- NULL
  if(!is.null(sexo)){
    q_sexo <- paste0(" AND " ,"def_sexo:", sexo)
  }

  q_diag_princ <- NULL
  if(!is.null(diag_princ)){
    q_diag_princ <- paste0(" AND " ,"DIAG_PRINC:\"", diag_princ, "\"")
  }

  q_diag_princ_capitulo <- NULL
  if(!is.null(diag_princ_capitulo)){
    q_diag_princ_capitulo <- paste0(" AND " ,"def_diag_princ_cap:\"", diag_princ_capitulo, "\"")
  }

  q_diag_princ_grupo <- NULL
  if(!is.null(diag_princ_grupo)){
    q_diag_princ_grupo <- paste0(" AND " ,"def_diag_princ_grupo:\"", diag_princ_grupo, "\"")
  }

  q_diag_princ_categoria <- NULL
  if(!is.null(diag_princ_categoria)){
    q_diag_princ_categoria <- paste0(" AND " ,"def_diag_princ_cat:\"", diag_princ_categoria, "\"")
  }

  q_diag_princ_subcategoria <- NULL
  if(!is.null(diag_princ_subcategoria)){
    q_diag_princ_subcategoria <- paste0(" AND " ,"def_diag_princ_subcat:\"", diag_princ_subcategoria, "\"")
  }

  q_idade_anos <- NULL
  if(!is.null(idade_anos_min)){
    q_idade_anos <- paste0(" AND " ,"def_idade_anos:[", idade_anos_min, " TO ", idade_anos_max, "]")
  }

  q_proced_real <- NULL
  if(!is.null(proced_real)){
    q_proced_real <- paste0(" AND " ,"PROC_REA:", proced_real)
  }

  q_lucene_query <- NULL
  if(!is.null(lucene_query)){
    q_lucene_query <- lucene_query
  }


  if(is.null(q_lucene_query)){
    query <- paste0(q_year, q_sexo, q_diag_princ, q_diag_princ_capitulo, q_diag_princ_grupo, q_diag_princ_categoria, q_diag_princ_subcategoria, q_idade_anos, q_proced_real)
  } else {
    query <- q_lucene_query
  }


  if(agr == "mun"){
    q_body <- agg_sih_mun
    df_names <- c("cod_mun", "sih")
  } else if (agr == "uf"){
    q_body <- agg_sih_uf
    df_names <- c("uf", "sih")
  } else if (agr == "regsaude"){
    q_body <- agg_sih_regsaude
    df_names <- c("cod_reg_saude", "sih")
  }


  # Request from ElasticSearch

  sim <- elastic::Search(conn, index="datasus-sih",
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
