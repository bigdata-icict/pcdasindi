####################################################################################
# Recebe um 'df' com uma coluna "cod_mun", e uma coluna-variável numérica 'var1'
# Devovle uma agregação da variável por 'cod_RS'
# Exemplo: df_pop <- pcdasindi::get_pop(conn, ano, "mun")
#          df <- agg_RS(df_pop, df_pop$pop, "populacao")
####################################################################################

agg_RS  <- function(df, var1, nome_var1)
{
url <- "https://raw.githubusercontent.com/caio-peixoto/principal/master/Lista_Cirs.csv"
download.file(url, destfile = "Lista_Cirs.csv", method = "curl")
RS <- fread("Lista_Cirs.csv", sep=";", encoding="UTF-8", select=c(1,2) ,colClasses = ("character"))
names(RS) <- c("cod_mun","cod_RS")

#df$cod_RS <- RS$cod_RS[ (df$cod_mun == RS$cod_mun)]
df <- merge(df, RS, by=c("cod_mun"))

#df <- df %>% group_by(cod_RS) %>% summarise(var = sum(arg1))
df <- aggregate(var1, by=list(cod_RS = df$cod_RS), FUN=sum)  
names(df) <-c("cod_RS", nome_var1)
    
url <- "https://raw.githubusercontent.com/caio-peixoto/principal/master/Lista_Cirs_RS.csv"
download.file(url, destfile = "Lista_Cirs_RS.csv", method = "curl")    
RS_438 <- fread("Lista_Cirs_RS.csv", sep=";", encoding="UTF-8", colClasses = ("character"))
names(RS_438) <- c("cod_RS","nome_RS")

#df$nome_RS <- RS_438$nome_RS[ (df$cod_RS == RS_438$cod_RS)]
df <- merge(df, RS_438, by=c("cod_RS"))
    
df <- df[,c(1,3,2)]

unlink("Lista_Cirs.csv")
unlink("Lista_Cirs_RS.csv")
rm(RS, RS_438)
    
return(df)   
}
