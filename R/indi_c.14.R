#'
#' Taxa de mortalidade específica por aids - C.14
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-14}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the municipalities IBGE codes (\code{cod_mun}) and the calculated indicator.
#' @examples
#' c.14 <- indi_c.14(conn, 2010)

indi_c.14 <- function(conn, ano, multi = 100000){

  categorias <- c(
    "B20   Doenc p/HIV result doenc infecc e parasit",
    "B21   Doenc p/HIV result em neopl malig",
    "B22   Doenc p/HIV result em outr doenc espec",
    "B23   Doenc p/HIV result em outr doenc",
    "B24   Doenc p/HIV NE"
  )

  sim <- data.frame()
  sim$cod_mun <- as.character()
  for(c in 1:length(categorias)){
    temp <- get_sim_mun(conn = conn, ano = ano, causabas_categoria = categorias[c])
    sim <- dplyr::full_join(sim, temp, by = c("cod_mun", "cod_mun"))
  }

  sums <- rowSums(sim[,-1], na.rm = TRUE)

  sim <- data.frame(cod_mun = sim$cod_mun, sim = sums)
  sim$cod_mun <- as.character(sim$cod_mun)

  pop <- get_pop_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, pop, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.14 = sim/pop*multi) %>%
    select(cod_mun, indi_c.14)

  return(df)
}
