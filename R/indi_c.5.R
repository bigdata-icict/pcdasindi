# Proporção de óbitos por causas mal definidas - C.5
indi_c.5 <- function(conn, ano, multi = 100){

  sim <- get_sim_mun(conn = conn, ano = ano, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")
  sim2 <- get_sim_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sim2, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.5 = sim.x/sim.y*multi) %>%
    select(cod_mun, indi_c.5)

  return(df)
}
