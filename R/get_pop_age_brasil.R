get_pop_age_brasil <- function(conn, ano){

  query <- paste0("ANO:", ano)

  pop <- elastic::Search(conn, index = "svs-pop-dss",
                         body = agg_pop_brasil_age, q = query, asdf = TRUE)

  nomes <- c("de5a9","de10a14","de15a19","de20a24","de25a29","de30a34","de35a39","de40a44","de45a49","de50a54","de55a59","de60a64","de65a69","de70a74","de75a79","de80","ate04")

  # Feminino
  fem <- pop[["aggregations"]][["3"]][["buckets"]][["2.buckets"]][[1]]
  fem <- data.frame(faixa = nomes, sexo = "Feminino", pop = fem$`1.value`, stringsAsFactors = FALSE)

  # Masculino
  masc <- pop[["aggregations"]][["3"]][["buckets"]][["2.buckets"]][[2]]
  masc <- data.frame(faixa = nomes, sexo = "Masculino", pop = masc$`1.value`, stringsAsFactors = FALSE)

  # Juntos
  df <- dplyr::bind_rows(list(fem, masc))

  return(df)
}
