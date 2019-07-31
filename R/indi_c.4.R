indi_c.4 <- function(conn, ano, multi = 100){

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


  sim <- data.frame()
  sim$cod_mun <- as.character()
  for(c in 1:length(capitulos)){
    temp <- get_sim_mun(conn = conn, ano = ano, causabas_capitulo =  capitulos[c])
    sim <- dplyr::full_join(sim, temp, by = c("cod_mun", "cod_mun"))
  }

  sums <- rowSums(sim[,8:ncol(sim)], na.rm = TRUE)
  sim$sums <- sums

  sim <- sim %>%
    dplyr::select(cod_mun, CapI = sim.x, CapII = sim.y, CapIX = sim.x.x, CapX = sim.y.y, CapXVI = sim.x.x.x, CapXX = sim.y.y.y.y, DemaisCausasMalDefinidas = sums)

  pop <- get_sim_mun(conn = conn, ano = ano)
  not_pop <- get_sim_mun(conn = conn, ano = ano, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")
  pop <- dplyr::full_join(pop, not_pop, by = c("cod_mun", "cod_mun")) %>%
    mutate(pop = sim.x - sim.y) %>%
    select(cod_mun, pop)

  df <- dplyr::left_join(sim, pop, by = c("cod_mun", "cod_mun")) %>%
    mutate(CapI = CapI/pop*multi,
           CapII = CapII/pop*multi,
           CapIX = CapIX/pop*multi,
           CapX = CapX/pop*multi,
           CapXVI = CapXVI/pop*multi,
           CapXX = CapXX/pop*multi,
           DemaisCausasMalDefinidas = DemaisCausasMalDefinidas/pop*multi) %>%
    mutate(Total = rowSums(.[2:8]))

  return(df)
}
