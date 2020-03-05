get_sim_age <- function(conn, ano, agr,
                        causabas = NULL,
                        causabas_capitulo = NULL,
                        causabas_grupo = NULL,
                        causabas_categoria = NULL,
                        causabas_subcategoria = NULL){

  if(agr == "mun"){
    df_names <- c("cod_mun")
  } else if (agr == "uf"){
    df_names <- c("uf")
  } else if (agr == "regsaude"){
    df_names <- c("cod_reg_saude")
  }

  # Feminino
  sim_f_ate04 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                       sexo = "Feminino", idade_obito_anos_min = 0, idade_obito_anos_max = 4)
  names(sim_f_ate04) <- c(df_names, "ate04")
  sim_f_de5a9 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                       sexo = "Feminino", idade_obito_anos_min = 5, idade_obito_anos_max = 9)
  names(sim_f_de5a9) <- c(df_names, "de5a9")
  sim_f_de10a14 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 10, idade_obito_anos_max = 14)
  names(sim_f_de10a14) <- c(df_names, "de10a14")
  sim_f_de15a19 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 15, idade_obito_anos_max = 19)
  names(sim_f_de15a19) <- c(df_names, "de15a19")
  sim_f_de20a24 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 20, idade_obito_anos_max = 24)
  names(sim_f_de20a24) <- c(df_names, "de20a24")
  sim_f_de25a29 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 25, idade_obito_anos_max = 29)
  names(sim_f_de25a29) <- c(df_names, "de25a29")
  sim_f_de30a34 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 30, idade_obito_anos_max = 34)
  names(sim_f_de30a34) <- c(df_names, "de30a34")
  sim_f_de35a39 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 35, idade_obito_anos_max = 39)
  names(sim_f_de35a39) <- c(df_names, "de35a39")
  sim_f_de40a44 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 40, idade_obito_anos_max = 44)
  names(sim_f_de40a44) <- c(df_names, "de40a44")
  sim_f_de45a49 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 45, idade_obito_anos_max = 49)
  names(sim_f_de45a49) <- c(df_names, "de45a49")
  sim_f_de50a54 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 50, idade_obito_anos_max = 54)
  names(sim_f_de50a54) <- c(df_names, "de50a54")
  sim_f_de55a59 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 55, idade_obito_anos_max = 59)
  names(sim_f_de55a59) <- c(df_names, "de55a59")
  sim_f_de60a64 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 60, idade_obito_anos_max = 64)
  names(sim_f_de60a64) <- c(df_names, "de60a64")
  sim_f_de65a69 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 65, idade_obito_anos_max = 69)
  names(sim_f_de65a69) <- c(df_names, "de65a69")
  sim_f_de70a74 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 70, idade_obito_anos_max = 74)
  names(sim_f_de70a74) <- c(df_names, "de70a74")
  sim_f_de75a79 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Feminino", idade_obito_anos_min = 75, idade_obito_anos_max = 79)
  names(sim_f_de75a79) <- c(df_names, "de75a79")
  sim_f_de80 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                      sexo = "Feminino", idade_obito_anos_min = 80, idade_obito_anos_max = 1000)
  names(sim_f_de80) <- c(df_names, "de80")

  list_sim <- list(sim_f_ate04, sim_f_de5a9, sim_f_de10a14, sim_f_de15a19, sim_f_de20a24, sim_f_de25a29, sim_f_de30a34, sim_f_de35a39, sim_f_de40a44, sim_f_de45a49, sim_f_de50a54, sim_f_de55a59, sim_f_de60a64, sim_f_de65a69, sim_f_de70a74, sim_f_de75a79, sim_f_de80)

  df_fem <- plyr::join_all(dfs = list_sim, type = "full", by = df_names) %>%
    dplyr::mutate(sexo = "Feminino") %>%
    tidyr::gather(faixa, sim, -df_names, -sexo)



  # Masculino
  sim_m_ate04 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Masculino", idade_obito_anos_min = 0, idade_obito_anos_max = 4)
  names(sim_m_ate04) <- c(df_names, "ate04")
  sim_m_de5a9 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                         sexo = "Masculino", idade_obito_anos_min = 5, idade_obito_anos_max = 9)
  names(sim_m_de5a9) <- c(df_names, "de5a9")
  sim_m_de10a14 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 10, idade_obito_anos_max = 14)
  names(sim_m_de10a14) <- c(df_names, "de10a14")
  sim_m_de15a19 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 15, idade_obito_anos_max = 19)
  names(sim_m_de15a19) <- c(df_names, "de15a19")
  sim_m_de20a24 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 20, idade_obito_anos_max = 24)
  names(sim_m_de20a24) <- c(df_names, "de20a24")
  sim_m_de25a29 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 25, idade_obito_anos_max = 29)
  names(sim_m_de25a29) <- c(df_names, "de25a29")
  sim_m_de30a34 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 30, idade_obito_anos_max = 34)
  names(sim_m_de30a34) <- c(df_names, "de30a34")
  sim_m_de35a39 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 35, idade_obito_anos_max = 39)
  names(sim_m_de35a39) <- c(df_names, "de35a39")
  sim_m_de40a44 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 40, idade_obito_anos_max = 44)
  names(sim_m_de40a44) <- c(df_names, "de40a44")
  sim_m_de45a49 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 45, idade_obito_anos_max = 49)
  names(sim_m_de45a49) <- c(df_names, "de45a49")
  sim_m_de50a54 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 50, idade_obito_anos_max = 54)
  names(sim_m_de50a54) <- c(df_names, "de50a54")
  sim_m_de55a59 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 55, idade_obito_anos_max = 59)
  names(sim_m_de55a59) <- c(df_names, "de55a59")
  sim_m_de60a64 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 60, idade_obito_anos_max = 64)
  names(sim_m_de60a64) <- c(df_names, "de60a64")
  sim_m_de65a69 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 65, idade_obito_anos_max = 69)
  names(sim_m_de65a69) <- c(df_names, "de65a69")
  sim_m_de70a74 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 70, idade_obito_anos_max = 74)
  names(sim_m_de70a74) <- c(df_names, "de70a74")
  sim_m_de75a79 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                           sexo = "Masculino", idade_obito_anos_min = 75, idade_obito_anos_max = 79)
  names(sim_m_de75a79) <- c(df_names, "de75a79")
  sim_m_de80 <- get_sim(conn = conn, ano = ano, agr = agr, causabas = causabas, causabas_capitulo = causabas_capitulo, causabas_grupo = causabas_grupo, causabas_categoria = causabas_categoria, causabas_subcategoria = causabas_subcategoria,
                        sexo = "Masculino", idade_obito_anos_min = 80, idade_obito_anos_max = 1000)
  names(sim_m_de80) <- c(df_names, "de80")

  list_sim <- list(sim_m_ate04, sim_m_de5a9, sim_m_de10a14, sim_m_de15a19, sim_m_de20a24, sim_m_de25a29, sim_m_de30a34, sim_m_de35a39, sim_m_de40a44, sim_m_de45a49, sim_m_de50a54, sim_m_de55a59, sim_m_de60a64, sim_m_de65a69, sim_m_de70a74, sim_m_de75a79, sim_m_de80)

  df_masc <- plyr::join_all(dfs = list_sim, type = "full", by = df_names) %>%
    dplyr::mutate(sexo = "Masculino") %>%
    tidyr::gather(faixa, sim, -df_names, -sexo)



  # Juntos
  df <- dplyr::bind_rows(list(df_fem, df_masc))

  return(df)
}
