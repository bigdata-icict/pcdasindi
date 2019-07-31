#'
#' Proporção de óbitos por causas mal definidas - C.5
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-5}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and the calculated indicator.
#' @examples
#' c.5 <- indi_c.5(conn, 2010)

indi_c.5 <- function(conn, ano, multi = 100){

  sim <- get_sim_mun(conn = conn, ano = ano, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")
  sim2 <- get_sim_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sim2, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.5 = sim.x/sim.y*multi) %>%
    select(cod_mun, indi_c.5)

  return(df)
}
