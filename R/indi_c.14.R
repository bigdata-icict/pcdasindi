#'
#' Taxa de mortalidade específica por aids - C.14
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-14}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.14 <- indi_c.14(conn, 2010, "mun")

indi_c.14 <- function(conn, ano, agr, multi = 100000){

  categorias <- c(
    "B20   Doenc p/HIV result doenc infecc e parasit",
    "B21   Doenc p/HIV result em neopl malig",
    "B22   Doenc p/HIV result em outr doenc espec",
    "B23   Doenc p/HIV result em outr doenc",
    "B24   Doenc p/HIV NE"
  )

  join_names <- join_names(agr = agr)

  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.14 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
