#' Get mortality data from SIM for several capítulos
#'
#' This function retrieves mortality data from PCDaS ElasticSearch cluster for a specified year and for several capítulos.
#'
#' This function uses raw data from the Sistema de Informações de Mortalidade (SIM) available at the PCDaS ElasticSearch cluster. A documentation about this data can be found at \url{https://bigdata-metadados.icict.fiocruz.br/dataset/sistema-de-informacoes-de-mortalidade-sim}.
#' The CID-10 codes and name for chapters, group, category and subcategories of basic cause of death can be inspected at the same address above.
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year of death.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param causabas_capitulos string vector of desired capítulos.
#'
#' @return A \code{data.frame} containing number of deceased (\code{sim}) for the aggregation level.
#' @examples
#' sim <- get_sim_capitulos(conn = conn, ano = 2010, agr = "mun", causabas_capitulos = c("I.   Algumas doenças infecciosas e parasitárias", "II.  Neoplasias (tumores)"))

get_sim_capitulos <- function(conn, ano, agr, causabas_capitulos){

  capitulos <- causabas_capitulos

  if(agr == "mun"){
    join_names <- c("cod_mun", "cod_mun")
    sim <- data.frame()
    sim$cod_mun <- as.character()
  } else if (agr == "uf"){
    join_names <- c("uf", "uf")
    sim <- data.frame()
    sim$uf <- as.character()
  } else if (agr == "regsaude"){
    join_names <- c("cod_reg_saude", "cod_reg_saude")
    sim <- data.frame()
    sim$cod_reg_saude <- as.character()
  }

  for(c in 1:length(capitulos)){
    temp <- get_sim(conn = conn, ano = ano, agr = agr, causabas_capitulo =  capitulos[c])
    sim <- dplyr::full_join(sim, temp, by = join_names)
  }

  sums <- rowSums(sim[,8:ncol(sim)], na.rm = TRUE)
  sim$sums <- sums

  return(sim)

}
