#'
#' Taxa de mortalidade específica por afecções originadas no período perinatal - C.15
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-15}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.15 <- indi_c.15(conn, 2010)

indi_c.15 <- function(conn, ano, agr, multi = 1000){

  sim <- get_sim(conn = conn, ano = ano, agr = agr, idade_obito_anos_min = 0, idade_obito_anos_max = 1, causabas_capitulo = "XVI. Algumas afec originadas no período perinatal")
  sinasc <- get_sinasc(conn = conn, ano = ano, agr = agr)

  if(agr == "mun"){
    join_names <- c("cod_mun", "cod_mun")
  } else if (agr == "uf"){
    join_names <- c("uf", "uf")
  } else if (agr == "regsaude"){
    join_names <- c("cod_reg_saude", "cod_reg_saude")
  }

  df <- dplyr::left_join(sim, sinasc, by = join_names) %>%
    mutate(indi_c.15 = sim/sinasc*multi) %>%
    select(1, 4)

  return(df)
}
