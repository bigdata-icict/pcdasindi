get_sim_mun <- function(conn, ano,
                        sexo = NULL,
                        causabas = NULL,
                        causabas_capitulo = NULL,
                        causabas_grupo = NULL,
                        causabas_categoria = NULL,
                        causabas_subcategoria = NULL,
                        idade_obito_anos_min = NULL, idade_obito_anos_max = NULL,
                        idade_obito_dias_min = NULL, idade_obito_dias_max = NULL){

  #source("R/agg_sim_mun.R", local = TRUE)

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


  # Request from ElasticSearch

  sim <- elastic::Search(conn, index="datasus-sim-dss",
                    body = agg_sim_mun,
                    q = query,
                    asdf = TRUE)

  sim <- sim$aggregations$a1$buckets
  if(is.null(nrow(sim))){ # No results
    sim <- data.frame(cod_mun = as.character(), sim = as.double()) # Empty data frame
    sim$cod_mun <- as.character()
  } else {
    names(sim) <- c("cod_mun", "sim")
  }

  return(sim)
}
