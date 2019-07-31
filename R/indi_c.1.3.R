# Taxa de mortalidade pós-neonatal - C.1.3
indi_c.1.3 <- function(conn, ano, multi = 1000){

  sim <- get_sim_mun(conn = conn, ano = ano, idade_obito_dias_min = 28, idade_obito_dias_max = 364)
  sinasc <- get_sinasc_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, sinasc, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.1.3 = sim/sinasc*multi) %>%
    select(cod_mun, indi_c.1.3)

  return(df)
}
