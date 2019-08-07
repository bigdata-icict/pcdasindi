#'
#' Proporção de óbitos por doença diarreica aguda em menores de 5 anos de idade - C.6
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-6}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.6 <- indi_c.6(conn, 2010, "mun")

indi_c.6 <- function(conn, ano, agr, multi = 100){

  categorias <- c(
    "A00   Colera",
    "A01   Febres tifoide e paratifoide",
    "A02   Outr infecc p/Salmonella",
    "A03   Shiguelose",
    "A04   Outr infecc intestinais bacter",
    "A05   Outr intox alimentares bacter NCOP",
    "A06   Amebiase",
    "A07   Outr doenc intestinais p/protozoarios",
    "A08   Infecc intestinais virais outr e as NE",
    "A09   Diarreia e gastroenterite orig infecc presum"
  )

  join_names <- join_names(agr = agr)

  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  sim2 <- get_sim(conn = conn, ano = ano, agr = agr, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")

  df <- dplyr::left_join(sim, sim2, by = join_names) %>%
    mutate(indi_c.6 = sim.x/sim.y*multi) %>%
    select(1, 4)

  return(df)
}
