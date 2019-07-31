# Taxa de mortalidade específica por doenças do aparelho circulatório - C.8
indi_c.8 <- function(conn, ano, multi = 100000){

  sim <- get_sim_mun(conn = conn, ano = ano, causabas_capitulo = "IX.  Doenças do aparelho circulatório")
  pop <- get_pop_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, pop, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.8 = sim/pop*multi) %>%
    select(cod_mun, indi_c.8)

  return(df)
}
