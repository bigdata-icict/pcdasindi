indi_c.15 <- function(conn, ano, multi = 100000){

  sim <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 1, causabas_capitulo = "XVI. Algumas afec originadas no período perinatal")
  sinasc <- get_sinasc_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sinasc, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.2 = sim/sinasc*multi) %>%
    select(cod_mun, indi_c.2)

  return(df)
}
