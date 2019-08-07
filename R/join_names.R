#'
#' Join names for aggregations
#'
#' It is a auxiliary function.
#'
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#'
#' @return Returns a string vector with join names for aggregation levels.
#' @examples
#' join_names <- join_names(agr = "mun")

join_names <- function(agr){
  if(agr == "mun"){
    join_names <- c("cod_mun", "cod_mun")
  } else if (agr == "uf"){
    join_names <- c("uf", "uf")
  } else if (agr == "regsaude"){
    join_names <- c("cod_reg_saude", "cod_reg_saude")
  }

  return(join_names)
}
