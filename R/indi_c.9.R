#'
#' Taxa de mortalidade específica por causas externas - C.9
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-9}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.9 <- indi_c.9(conn, 2010, "mun")

indi_c.9 <- function(conn, ano, agr, multi = 100000){

  if(agr == "mun"){
    join_names <- c("cod_mun", "cod_mun")
  } else if (agr == "uf"){
    join_names <- c("uf", "uf")
  } else if (agr == "regsaude"){
    join_names <- c("cod_reg_saude", "cod_reg_saude")
  }

  sim <- get_sim(conn = conn, ano = ano, agr = agr, causabas_capitulo = "XX.  Causas externas de morbidade e mortalidade")
  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.9 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
