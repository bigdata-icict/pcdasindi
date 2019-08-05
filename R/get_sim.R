#' Get mortality data for municipalities
#'
#' This function retrieves mortality data from PCDaS ElasticSearch cluster for a specified year.
#'
#' This function uses raw data from the Sistema de Informações de Mortalidade (SIM) available at the PCDaS ElasticSearch cluster. A documentation about this data can be found at \url{https://bigdata-metadados.icict.fiocruz.br/dataset/sistema-de-informacoes-de-mortalidade-sim}.
#' The CID-10 codes and name for chapters, group, category and subcategories of basic cause of death can be inspected at the same address above.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year of death.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param sexo string. Sex of the deceased. \code{Masculino} for males or \code{Feminino} for females.
#' @param causabas string. CID-10 code for the basic cause of death.
#' @param causabas_capitulo string. Chapter of the basic cause of death.
#' @param causabas_grupo string. Group of the basic cause of death.
#' @param causabas_categoria string. Category of the basic cause of death.
#' @param causabas_subcategoria string. Subcategory of the basic cause of death.
#' @param idade_obito_anos_min numeric. Minimum age of death, in years.
#' @param idade_obito_anos_max numeric. Maximum age of death, in years.
#' @param idade_obito_dias_min numeric. Minimum age of death, in days.
#' @param idade_obito_dias_max numeric. Maximum age of death, in days.
#'
#' @return A \code{data.frame} containing number of deceased (\code{sim}) for the aggregation level.
#' @examples
#' sim <- get_sim_mun(conn = conn, ano = 2010, agr = "mun", sexo = "Masculino")
#' sim <- get_sim_mun(conn = conn, ano = 2010, agr = "mun", causabas = "R98")
#' sim <- get_sim_mun(conn = conn, ano = 2010, agr = "mun", causabas_categoria = "O01   Mola hidatiforme")
#' sim <- get_sim_mun(conn = conn, ano = 2010, agr = "mun", idade_obito_anos_min = 0, idade_obito_anos_max = 1)
#' sim <- get_sim_mun(conn = conn, ano = 2010, agr = "mun", idade_obito_dias_min = 0, idade_obito_dias_max = 1)

get_sim <- function(conn, ano, agr,
                        sexo = NULL,
                        causabas = NULL,
                        causabas_capitulo = NULL,
                        causabas_grupo = NULL,
                        causabas_categoria = NULL,
                        causabas_subcategoria = NULL,
                        idade_obito_anos_min = NULL, idade_obito_anos_max = NULL,
                        idade_obito_dias_min = NULL, idade_obito_dias_max = NULL){

  # Queries

  q_year <- paste0("ano_obito:", ano)

  q_sexo <- NULL
  if(!is.null(sexo)){
    q_sexo <- paste0(" AND " ,"def_sexo:", sexo)
  }

  q_causabas <- NULL
  if(!is.null(causabas)){
    q_causabas <- paste0(" AND " ,"CAUSABAS:\"", causabas, "\"")
  }

  q_causabas_capitulo <- NULL
  if(!is.null(causabas_capitulo)){
    q_causabas_capitulo <- paste0(" AND " ,"causabas_capitulo:\"", causabas_capitulo, "\"")
  }

  q_causabas_grupo <- NULL
  if(!is.null(causabas_grupo)){
    q_causabas_grupo <- paste0(" AND " ,"causabas_grupo:\"", causabas_grupo, "\"")
  }

  q_causabas_categoria <- NULL
  if(!is.null(causabas_categoria)){
    q_causabas_categoria <- paste0(" AND " ,"causabas_categoria:\"", causabas_categoria, "\"")
  }

  q_causabas_subcategoria <- NULL
  if(!is.null(causabas_subcategoria)){
    q_causabas_subcategoria <- paste0(" AND " ,"causabas_subcategoria:\"", causabas_subcategoria, "\"")
  }

  q_idade_obito_anos <- NULL
  if(!is.null(idade_obito_anos_min)){
    q_idade_obito_anos <- paste0(" AND " ,"idade_obito_calculado:", idade_obito_anos_min, " to ", idade_obito_anos_max)
  }

  q_idade_obito_dias <- NULL
  if(!is.null(idade_obito_dias_min)){
    q_idade_obito_dias <- paste0(" AND " ,"idade_obito_dias:", idade_obito_dias_min, " to ", idade_obito_dias_max)
  }

  query <- paste0(q_year, q_sexo, q_causabas, q_causabas_capitulo, q_causabas_grupo, q_causabas_categoria, q_causabas_subcategoria, q_idade_obito_anos, q_idade_obito_dias)



  if(agr == "mun"){
    q_body <- agg_sim_mun
    df_names <- c("cod_mun", "sim")
  } else if (agr == "uf"){
    q_body <- agg_sim_uf
    df_names <- c("uf", "sim")
  } else if (agr == "regsaude"){
    q_body <- agg_sim_regsaude
    df_names <- c("cod_reg_saude", "sim")
  }


  # Request from ElasticSearch

  sim <- elastic::Search(conn, index="datasus-sim-dss",
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
