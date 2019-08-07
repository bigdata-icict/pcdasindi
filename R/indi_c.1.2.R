#'
#' Taxa de mortalidade neonatal tardia - C.1.2
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-1-2}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.1.2 <- indi_c.1.2(conn = conn, ano = 2010, agr = "mun")

indi_c.1.2 <- function(conn, ano, agr, multi = 1000){

  sim <- get_sim(conn = conn, ano = ano, agr = agr, idade_obito_dias_min = 7, idade_obito_dias_max = 27)
  sinasc <- get_sinasc(conn = conn, ano = ano, agr = agr)

  join_names <- join_names(agr = agr)

  df <- dplyr::left_join(sim, sinasc, by = join_names) %>%
    mutate(indi_c.1.2 = sim/sinasc*multi) %>%
    select(1, 4)

  return(df)
}
