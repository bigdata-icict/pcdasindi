######################################################################################################
# Essa função recebe um df com uma coluna chamada **'cod_mun'** de **6 dígitos** tipo 'character', 
# e retorna esse 'df' com 2 colunas a mais: Código de Região de Saude (CIR) - **'cod_RS'**, de **5 dígitos**, 
# e uma coluna como o 'Nome da Região de Saúde '**nome_RS**' 
# EX: df_pop_16 <- pcdasindi::get_pop(conn, ano, "mun")
#     df <- converte_RS(df_pop_16) 
########################################################################################################

converte_RS <- function(df)
{
url <- "https://raw.githubusercontent.com/caio-peixoto/principal/master/Lista_Cirs.csv"
download.file(url, destfile = "Lista_Cirs.csv")
RS <- fread("Lista_Cirs.csv", sep=";", encoding="UTF-8", colClasses = ("character"))

names(RS) <- c("cod_mun","cod_RS","cod_UF","sigla_UF","nome_UF","nome_RS","nome_mun")

df$cod_RS <- RS$cod_RS[ (df$cod_mun == RS$cod_mun)]
df$nome_RS <- RS$nome_RS[ (df$cod_mun == RS$cod_mun)]

unlink("Lista_Cirs.csv")
rm(RS)
    
return(df)
}
