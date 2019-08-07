#'
#' Taxa de mortalidade específica por doenças do aparelho circulatório - C.8
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-8}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.8 <- indi_c.8(conn, 2010, "mun")

indi_c.8 <- function(conn, ano, agr, multi = 100000){

  join_names <- join_names(agr = agr)

  sim <- get_sim(conn = conn, ano = ano, agr = agr, causabas_capitulo = "IX.  Doenças do aparelho circulatório")
  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.8 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
