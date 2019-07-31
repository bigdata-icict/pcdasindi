indi_c.14 <- function(conn, ano, multi = 100){

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
    mutate(indi_c.2 = sim/pop*multi) %>%
    select(cod_mun, indi_c.2)

  return(df)
}
