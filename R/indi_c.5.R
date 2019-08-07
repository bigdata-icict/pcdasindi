#'
#' Proporção de óbitos por causas mal definidas - C.5
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-5}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.5 <- indi_c.5(conn, 2010, "mun")

indi_c.5 <- function(conn, ano, agr, multi = 100){

  sim <- get_sim(conn = conn, ano = ano, agr = agr, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")
  sim2 <- get_sim(conn = conn, ano = ano, agr = agr)

  join_names <- join_names(agr = agr)

  df <- dplyr::left_join(sim, sim2, by = join_names) %>%
    mutate(indi_c.5 = sim.x/sim.y*multi) %>%
    select(1, 3)

  return(df)
}
