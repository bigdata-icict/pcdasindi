#'
#' Razão de mortalidade materna - C.3
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-3}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.3 <- indi_c.3(conn, 2010, "mun")

indi_c.3 <- function(conn, ano, agr, multi = 100000){

  categorias <- c(
    "O00   Gravidez ectopica",
    "O01   Mola hidatiforme",
    "O02   Outr produtos anormais da concepcao",
    "O03   Aborto espontaneo",
    "O04   Aborto p/razoes medicas e legais",
    "O05   Outr tipos de aborto",
    "O06   Aborto NE",
    "O07   Falha de tentativa de aborto",
    "O10   Hipertens pre-exist complic grav parto puerp",
    "O11   Dist hipertens pre-exist proteinuria superp",
    "O12   Edema e proteinuria gestac s/hipertensao",
    "O13   Hipertensao gestacional s/proteinuria signif",
    "O14   Hipertensao gestacional c/proteinuria signif",
    "O15   Eclampsia",
    "O16   Hipertensao materna NE",
    "O20   Hemorragia do inicio da gravidez",
    "O21   Vomitos excessivos na gravidez",
    "O22   Complic venosas na gravidez",
    "O23   Infecc do trato geniturinario na gravidez",
    "O24   Diabetes mellitus na gravidez",
    "O26   Assist materna outr complic lig predom grav",
    "O30   Gestacao mult",
    "O34   Assist prest mae anor conh susp org pelv mat",
    "O36   Assist prest mae outr probl fet conhec susp",
    "O41   Outr transt membranas e liquido amniotico",
    "O42   Ruptura prematura de membranas",
    "O43   Transt da placenta",
    "O44   Placenta previa",
    "O45   Descolamento prematuro da placenta",
    "O46   Hemorragia anteparto NCOP",
    "O47   Falso trabalho de parto",
    "O48   Gravidez prolongada",
    "O60   Trabalho de parto pre-termo",
    "O62   Anormalidades da contracao uterina",
    "O65   Obstr trab parto dev anorm pelvica da mae",
    "O66   Outr form de obstrucao do trabalho de parto",
    "O67   Trab parto parto compl hemorr intrapart NCOP",
    "O69   Trab parto parto compl anorm cordao umbilic",
    "O70   Laceracao do perineo durante o parto",
    "O71   Outr traum obstetricos",
    "O72   Hemorragia pos-parto",
    "O73   Retencao placenta e membranas s/hemorragias",
    "O74   Complic anestesia durante trab parto e parto",
    "O75   Outr complic do trab parto e do parto NCOP",
    "O85   Infecc puerperal",
    "O86   Outr infecc puerperais",
    "O87   Complic venosas no puerperio",
    "O88   Embolia orig obstetrica",
    "O89   Complic da anestesia admin durante puerperio",
    "O90   Complic do puerperio NCOP",
    "O91   Infecc mamarias assoc ao parto",
    "O95   Morte obstetrica de causa NE",
    #"O96   Morte qq caus obst mais 42d menos 1a parto",
    #"O97   Morte p/sequelas causas obstetricas diretas",
    "O98   Doen inf paras mat COP compl grav part puerp",
    "O99   Outr doenc mat COP compl grav parto puerp",
    "A34   Tetano obstetrico",
    "F53   Transt mentais comport assoc puerperio NCOP",
    "M83   Osteomalacia do adulto"
  )


  join_names <- join_names(agr = agr)
  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  sinasc <- get_sinasc(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, sinasc, by = join_names) %>%
    mutate(indi_c.3 = sim/sinasc*multi) %>%
    select(1, 4)

  return(df)
}
