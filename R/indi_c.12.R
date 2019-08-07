#'
#' Taxa de mortalidade específica por diabete melito - C.12
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-12}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.12 <- indi_c.12(conn, 2010, "mun")

indi_c.12 <- function(conn, ano, agr, multi = 100000){

  categorias <- c(
    "E10   Diabetes mellitus insulino-dependente",
    "E11   Diabetes mellitus nao-insulino-dependemte",
    "E12   Diabetes mellitus relac c/a desnutr",
    "E13   Outr tipos espec de diabetes mellitus",
    "E14   Diabetes mellitus NE"
  )

  join_names <- join_names(agr = agr)

  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.12 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
