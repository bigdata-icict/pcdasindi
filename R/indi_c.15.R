#'
#' Taxa de mortalidade específica por afecções originadas no período perinatal - C.15
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-15}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and the calculated indicator.
#' @examples
#' c.15 <- indi_c.15(conn, 2010)

indi_c.15 <- function(conn, ano, multi = 1000){

  sim <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 1, causabas_capitulo = "XVI. Algumas afec originadas no período perinatal")
  sinasc <- get_sinasc_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sinasc, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.15 = sim/sinasc*multi) %>%
    select(cod_mun, indi_c.15)

  return(df)
}
