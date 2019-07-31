# Taxa de mortalidade neonatal tardia - C.1.2
indi_c.1.2 <- function(conn, ano, multi = 1000){

  sim <- get_sim_mun(conn = conn, ano = ano, idade_obito_dias_min = 7, idade_obito_dias_max = 27)
  sinasc <- get_sinasc_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sinasc, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.1.2 = sim/sinasc*multi) %>%
    select(cod_mun, indi_c.1.2)

  return(df)
}
