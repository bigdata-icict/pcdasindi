####################################################################################
# Recebe um 'df' com uma coluna "cod_mun", e apaga as linhas com cod_mun ignorados
# mantendo um número máx de 5570 municípios por ano.
# Exemplo:  sinasc <- get_sinasc(conn, ano, "mun")
#           sinasc <- regras_mun(sinasc)
####################################################################################

regras_mun  <- function(df)
{
    
   url <- "https://raw.githubusercontent.com/caio-peixoto/principal/master/ignorados.csv"
download.file(url, destfile = "ignorados.csv", method = "curl")

ignorados <- read.table("ignorados.csv", stringsAsFactors = F, header = T, sep = ";", colClasses = "character")

names(ignorados) <- c("cod_mun","cgr","uf") 
    
    df<-anti_join(df, ignorados, by = "cod_mun")
    df <- df[with(df, order(cod_mun)),]
    
    unlink("ignorados.csv")
rm(ignorados)

  return(df)  
    }
