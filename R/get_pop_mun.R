get_pop_mun <- function(conn, ano){

  #source("R/agg_pop_full_mun.R", local = TRUE)

  query <- paste0("ANO:", ano)

  pop <- elastic::Search(conn, index="svs-pop-dss",
                    body = agg_pop_mun,
                    q = query,
                    asdf = TRUE)

  pop <- pop$aggregations$a1$buckets
  pop$doc_count <- NULL
  names(pop) <- c("cod_mun", "pop")

  return(pop)
}
