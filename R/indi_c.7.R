# Proporção de óbitos por infecção respiratória aguda em menores de 5 anos de idade - C.7
indi_c.7 <- function(conn, ano, multi = 100){

  categorias <- c(
    "J00   Nasofaringite aguda",
    "J01   Sinusite aguda",
    "J02   Faringite aguda",
    "J03   Amigdalite aguda",
    "J04   Laringite e traqueite agudas",
    "J05   Laringite obstrutiva aguda e epiglotite",
    "J06   Infecc agudas vias aereas super loc mult NE",
    "J09   Influenza dev virus gripe aviária",
    "J10   Influenza dev outro virus influenza ident",
    "J11   Influenza dev virus nao identificado",
    "J12   Pneumonia viral NCOP",
    "J13   Pneumonia dev Streptococcus pneumoniae",
    "J14   Pneumonia dev Haemophilus infuenzae",
    "J15   Pneumonia bacter NCOP",
    "J16   Pneumonia dev out microorg infecc espec NCOP",
    "J18   Pneumonia p/microorg NE",
    "J20   Bronquite aguda",
    "J21   Bronquiolite aguda",
    "J22   Infecc agudas NE das vias aereas infer"
  )

  sim <- data.frame()
  sim$cod_mun <- as.character()
  for(c in 1:length(categorias)){
    temp <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_categoria = categorias[c])
    sim <- dplyr::full_join(sim, temp, by = c("cod_mun", "cod_mun"))
  }

  sums <- rowSums(sim[,-1], na.rm = TRUE)

  sim <- data.frame(cod_mun = sim$cod_mun, sim = sums)
  sim$cod_mun <- as.character(sim$cod_mun)

  sim2 <- get_sim_mun(conn = conn, ano = ano, idade_obito_anos_min = 0, idade_obito_anos_max = 5, causabas_capitulo = "XVIII.Sint sinais e achad anorm ex clín e laborat")

  df <- dplyr::left_join(sim, sim2, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.7 = sim.x/sim.y*multi) %>%
    select(cod_mun, indi_c.7)

  return(df)
}
