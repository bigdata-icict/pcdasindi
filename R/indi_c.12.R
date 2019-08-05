#'
#' Taxa de mortalidade específica por diabete melito - C.12
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-12}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agg string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
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

  if(agr == "mun"){
    join_names <- c("cod_mun", "cod_mun")
    sim <- data.frame()
    sim$cod_mun <- as.character()
  } else if (agr == "uf"){
    join_names <- c("uf", "uf")
    sim <- data.frame()
    sim$uf <- as.character()
  } else if (agr == "regsaude"){
    join_names <- c("cod_reg_saude", "cod_reg_saude")
    sim <- data.frame()
    sim$cod_reg_saude <- as.character()
  }


  for(c in 1:length(categorias)){
    temp <- get_sim(conn = conn, ano = ano, agr = agr, causabas_categoria = categorias[c])
    if(nrow(temp) == 0) {
      next
    } else {
      sim <- dplyr::full_join(sim, temp, by = join_names)
    }
  }

  sums <- rowSums(sim[,-1], na.rm = TRUE)

  sim <- sim %>% mutate(sim = sums)
  sim <- sim %>% select(1, length(sim))

  sim[,1] <- as.character(sim[,1])

  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.12 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
