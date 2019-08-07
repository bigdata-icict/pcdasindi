#'
#' Mortalidade proporcional por grupos de causas - C.4
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-4}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.4 <- indi_c.4(conn, 2010, "mun")

indi_c.4 <- function(conn, ano, agr, multi = 100){

  capitulos <- c(
    "I.   Algumas doenças infecciosas e parasitárias",
    "II.  Neoplasias (tumores)",
    "IX.  Doenças do aparelho circulatório",
    "X.   Doenças do aparelho respiratório",
    "XVI. Algumas afec originadas no período perinatal",
    "XX.  Causas externas de morbidade e mortalidade",
    "III. Doenças sangue órgãos hemat e transt imunitár",
    "IV.  Doenças endócrinas nutricionais e metabólicas",
    "V.   Transtornos mentais e comportamentais",
    "VI.  Doenças do sistema nervoso",
    "VII. Doenças do olho e anexos",
    "VIII.Doenças do ouvido e da apófise mastóide",
    "XI.  Doenças do aparelho digestivo",
    "XII. Doenças da pele e do tecido subcutâneo",
    "XIII.Doenças sist osteomuscular e tec conjuntivo",
    "XIV. Doenças do aparelho geniturinário",
    "XV.  Gravidez parto e puerpério",
    "XVII.Malf cong deformid e anomalias cromossômicas"
  )

  sim <- get_sim_capitulos(conn = conn, ano = ano, agr = agr, causabas_capitulos = capitulos)
  sim <- sim %>%
    dplyr::select(1, CapI = sim.x, CapII = sim.y, CapIX = sim.x.x, CapX = sim.y.y, CapXVI = sim.x.x.x, CapXX = sim.y.y.y.y, DemaisCausasMalDefinidas = sums)

  join_names <- join_names(agr = agr)

  pop <- get_sim(conn = conn, ano = ano, agr = agr)
  not_pop <- get_sim(conn = conn, ano = ano, agr = agr, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")
  pop <- dplyr::full_join(pop, not_pop, by = join_names) %>%
    mutate(pop = sim.x - sim.y) %>%
    select(1, pop)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(CapI = CapI/pop*multi,
           CapII = CapII/pop*multi,
           CapIX = CapIX/pop*multi,
           CapX = CapX/pop*multi,
           CapXVI = CapXVI/pop*multi,
           CapXX = CapXX/pop*multi,
           DemaisCausasMalDefinidas = DemaisCausasMalDefinidas/pop*multi) %>%
    mutate(Total = rowSums(.[2:8])) %>%
    select(-pop)

  return(df)
}
