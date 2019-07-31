#'
#' Proporção de óbitos por doença diarreica aguda em menores de 5 anos de idade - C.6
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-6}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and the calculated indicator.
#' @examples
#' c.6 <- indi_c.6(conn, 2010)

indi_c.6 <- function(conn, ano, multi = 100){

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

  sim <- data.frame()
  sim$cod_mun <- as.character()
  for(c in 1:length(categorias)){
    temp <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_categoria = categorias[c])
    sim <- dplyr::full_join(sim, temp, by = c("cod_mun", "cod_mun"))
  }

  sums <- rowSums(sim[,-1], na.rm = TRUE)

  sim <- data.frame(cod_mun = sim$cod_mun, sim = sums)
  sim$cod_mun <- as.character(sim$cod_mun)

  sim2 <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")

  df <- dplyr::left_join(sim, sim2, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.6 = sim.x/sim.y*multi) %>%
    select(cod_mun, indi_c.6)

  return(df)
}
