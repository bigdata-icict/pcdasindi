#'
#' Taxa de mortalidade específica por doenças transmissíveis - C.17
#'
#' Follows the RIPSA 2012 card \url{http://fichas.ripsa.org.br/2012/c-17}
#'
#' @param conn Connection object created with \code{\link{pcdas_connect}}.
#' @param ano numeric. Year.
#' @param agr string. Aggregation level. 'mun' for municipalities, 'uf' for "unidades federativas" or 'regsaude' for "regiões de saúde".
#' @param multi Indicator multiplier. Defaults to RIPSA recommendation.
#'
#' @return A \code{data.frame} containing the calculated indicator for the aggregation level.
#' @examples
#' c.17 <- indi_c.17(conn, ano = 2016, agr = "mun")

indi_c.17 <- function(conn, ano, agr, multi = 100000){

  categorias <- c(
    "A00   Colera",
    "A01   Febres tifoide e paratifoide",
    "A02   Outr infecc p/Salmonella",
    "A03   Shiguelose",
    "A04   Outr infecc intestinais bacter",
    "A05   Outr intox alimentares bacter NCOP",
    "A06   Amebiase",
    "A07   Outr doenc intestinais p/protozoarios",
    "A08   Infecc intestinais virais outr e as NE",
    "A09   Diarreia e gastroenterite orig infecc presum",
    "A15   Tuberc respirat c/conf bacteriol e histolog",
    "A16   Tuberc vias respirat s/conf bacter histol",
    "A17   Tuberc do sist nervoso",
    "A18   Tuberc de outr orgaos",
    "A19   Tuberc miliar",
    "A21   Tularemia",
    "A22   Carbunculo",
    "A23   Brucelose",
    "A24   Mormo e melioidose",
    "A25   Febres transm p/mordedura de rato",
    "A26   Erisipeloide",
    "A27   Leptospirose",
    "A28   Outr doenc bacter zoonoticas NCOP",
    "A30   Hanseniase",
    "A31   Infecc dev outr micobacterias",
    "A32   Listeriose",
    "A33   Tetano do recem-nascido",
    "A34   Tetano obstetrico",
    "A35   Outr tipos de tetano",
    "A36   Difteria",
    "A37   Coqueluche",
    "A38   Escarlatina",
    "A39   Infecc meningogocica",
    "A40   Septicemia estreptococica",
    "A41   Outr septicemias",
    "A42   Actinomicose",
    "A43   Nocardiose",
    "A44   Bartonelose",
    "A46   Erisipela",
    "A48   Outr doenc bacter NCOP",
    "A49   Infecc bacter de localiz NE",
    "A50   Sifilis congen",
    "A51   Sifilis precoce",
    "A52   Sifilis tard",
    "A53   Outr form e as NE da sifilis",
    "A54   Infecc gonococica",
    "A55   Linfogranuloma p/clamidia",
    "A56   Outr infecc p/clamidias transm p/via sexual",
    "A57   Cancro mole",
    "A58   Granuloma inguinal",
    "A59   Tricomoniase",
    "A60   Infecc anogenitais p/virus do herpes",
    "A63   Outr doenc de transm predom sexual NCOP",
    "A64   Doenc sexualmente transm NE",
    "A65   Sifilis nao-venerea",
    "A66   Bouba",
    "A67   Pinta",
    "A68   Febres recorrentes",
    "A69   Outr infecc p/espiroquetas",
    "A70   Infecc causadas p/Clamidia psittaci",
    "A71   Tracoma",
    "A74   Outr doenc causadas p/Clamidias",
    "A75   Tifo exantematico",
    "A77   Febre maculosa",
    "A79   Outr rickettsioses",
    "A81   Infecc p/virus atipicos sist nervoso central",
    "A82   Raiva",
    "A83   Encefalite p/virus transm p/mosquitos",
    "A84   Encefalite p/virus transm p/carrapatos",
    "A85   Outr encefalites virais NCOP",
    "A86   Encefalite viral NE",
    "A87   Meningite viral",
    "A88   Outr infecc virais sist nervoso central NCOP",
    "A89   Infecc virais NE do sist nervoso central",
    "A90   Dengue",
    "A91   Febre hemorragica dev virus do dengue",
    "A92   Outr febres virais transm p/mosquitos",
    "A93   Outr febres p/virus transm p/artropodes NCOP",
    "A94   Febre viral transm p/artropodes NE",
    "A95   Febre amarela",
    "A96   Febre hemorragica p/arenavirus",
    "A98   Outr febres hemorragicas p/virus NCOP",
    "A99   Febres hemorragicas virais NE",
    "B00   Infecc p/virus do herpes",
    "B01   Varicela",
    "B02   Herpes zoster",
    "B05   Sarampo",
    "B06   Rubeola",
    "B07   Verrugas orig viral",
    "B08   Outr inf virais lesoes pele membr muc NCOP",
    "B09   Inf viral NE caract p/lesoes pele membr muco",
    "B15   Hepatite aguda A",
    "B16   Hepatite aguda B",
    "B17   Outr hepatites virais agudas",
    "B18   Hepatite viral cronica",
    "B19   Hepatite viral NE",
    "B20   Doenc p/HIV result doenc infecc e parasit",
    "B21   Doenc p/HIV result em neopl malig",
    "B22   Doenc p/HIV result em outr doenc espec",
    "B23   Doenc p/HIV result em outr doenc",
    "B24   Doenc p/HIV NE",
    "B25   Doenc p/citomegalovirus",
    "B26   Caxumba",
    "B27   Mononucleose infecc",
    "B30   Conjuntivite viral",
    "B33   Outr doenc p/virus NCOP",
    "B34   Doenc p/virus de localiz NE",
    "B35   Dermatofitose",
    "B36   Outr micoses superf",
    "B37   Candidiase",
    "B38   Coccidioidomicose",
    "B39   Histoplasmose",
    "B40   Blastomicose",
    "B41   Paracoccidioidomicose",
    "B42   Esporotricose",
    "B43   Cromomicose e abscesso feomicotico",
    "B44   Aspergilose",
    "B45   Criptococose",
    "B46   Zigomicose",
    "B47   Micetoma",
    "B48   Outr micoses NCOP",
    "B49   Micose NE",
    "B50   Malaria p/Plasmodium falciparum",
    "B51   Malaria p/Plasmodium vivax",
    "B52   Malaria p/Plasmodium malariae",
    "B53   Outr form malaria conf p/exames parasitolog",
    "B54   Malaria NE",
    "B55   Leishmaniose",
    "B56   Tripanossomiase africana",
    "B57   Doenc de Chagas",
    "B58   Toxoplasmose",
    "B59   Pneumocistose",
    "B60   Outr doenc dev protozoarios NCOP",
    "B64   Doenc NE dev protozoarios",
    "B65   Esquistossomose",
    "B66   Outr infestacoes p/trematodeos",
    "B67   Equinococose",
    "B68   Infestacao p/Taenia",
    "B69   Cisticercose",
    "B71   Outr infestacoes p/cestoides",
    "B72   Dracontiase",
    "B73   Oncocercose",
    "B74   Filariose",
    "B75   Triquinose",
    "B76   Ancilostomiase",
    "B77   Ascaridiase",
    "B78   Estrongiloidiase",
    "B79   Tricuriase",
    "B80   Oxiuriase",
    "B81   Outr helmintiases intestinais NCOP",
    "B82   Parasitose intestinal NE",
    "B83   Outr helmintiases",
    "B85   Pediculose e ftiriase",
    "B86   Escabiose",
    "B87   Miiase",
    "B88   Outr infestacoes",
    "B89   Doenc parasit NE",
    "B90   Sequelas de tuberc",
    "B91   Sequelas de poliomielite",
    "B92   Sequelas de hanseniase",
    "B94   Sequelas de outr doenc infecc e parasit e NE",
    "B95   Estr/estafilococo causa doenc class outr cap",
    "B96   Outr agent bacter causa doenc class outr cap",
    "B97   Virus como doenc class outr cap",
    "B99   Doenc infecc outr e as NE",
    "G00   Meningite bacter NCOP",
    "G03   Meningite dev outr causas e a causas NE",
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

  join_names <- join_names(agr = agr)

  sim <- get_sim_categorias(conn = conn, ano = ano, agr = agr, causabas_categorias = categorias)
  pop <- get_pop(conn = conn, ano = ano, agr = agr)

  df <- dplyr::left_join(sim, pop, by = join_names) %>%
    mutate(indi_c.17 = sim/pop*multi) %>%
    select(1, 4)

  return(df)
}
