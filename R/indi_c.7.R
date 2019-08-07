#'
#' Proporção de óbitos por infecção respiratória aguda em menores de 5 anos de idade - C.7
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-7}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.7 <- indi_c.7(conn, 2010, "mun")

indi_c.7 <- function(conn, ano, agr, multi = 100){

  categorias <- c(
    "J00   Nasofaringite aguda",
    "J01   Sinusite aguda",
    "J02   Faringite aguda",
    "J03   Amigdalite aguda",
    "J04   Laringite e traqueite agudas",
    "J05   Laringite obstrutiva aguda e epiglotite",
    "J06   Infecc agudas vias aereas super loc mult NE",
    "J09   Influenza dev virus gripe aviária",
    "J10   Influenza dev outro virus influenza ident",
    "J11   Influenza dev virus nao identificado",
    "J12   Pneumonia viral NCOP",
    "J13   Pneumonia dev Streptococcus pneumoniae",
    "J14   Pneumonia dev Haemophilus infuenzae",
    "J15   Pneumonia bacter NCOP",
    "J16   Pneumonia dev out microorg infecc espec NCOP",
    "J18   Pneumonia p/microorg NE",
    "J20   Bronquite aguda",
    "J21   Bronquiolite aguda",
    "J22   Infecc agudas NE das vias aereas infer"
  )

  join_names <- join_names(agr = agr)

  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  sim2 <- get_sim(conn = conn, ano = ano, agr = agr, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")

  df <- dplyr::left_join(sim, sim2, by = join_names) %>%
    mutate(indi_c.7 = sim.x/sim.y*multi) %>%
    select(1, 4)

  return(df)
}
