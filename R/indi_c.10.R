# Taxa de mortalidade específica por neoplasias malignas - C.10
indi_c.10 <- function(conn, ano, multi = 100000){

  categorias <- c(
    "C00   Neopl malig do labio",
    "C01   Neopl malig da base da lingua",
    "C02   Neopl malig outr partes e NE da lingua",
    "C03   Neopl malig da gengiva",
    "C04   Neopl malig do assoalho da boca",
    "C05   Neopl malig do palato",
    "C06   Neopl malig outr partes e partes NE da boca",
    "C07   Neopl malig da gland parotida",
    "C08   Neopl malig outr gland saliv maiores e NE",
    "C09   Neopl malig da amigdala",
    "C10   Neopl malig da orofaringe",
    "C11   Neopl malig da nasofaringe",
    "C12   Neopl malig do seio piriforme",
    "C13   Neopl malig da hipofaringe",
    "C14   Neop mal out loc mal def labio cav oral far",
    "C15   Neopl malig do esofago",
    "C16   Neopl malig do estomago",
    "C17   Neopl malig do intestino delgado",
    "C18   Neopl malig do colon",
    "C19   Neopl malig da juncao retossigmoide",
    "C20   Neopl malig do reto",
    "C21   Neopl malig do anus e do canal anal",
    "C22   Neopl malig figado vias biliares intra-hepat",
    "C23   Neopl malig da vesicula biliar",
    "C24   Neopl malig outr partes e NE vias biliares",
    "C25   Neopl malig do pancreas",
    "C26   Neopl malig outr mal def aparelho digestivo",
    "C30   Neopl malig cavidade nasal e do ouvido medio",
    "C31   Neopl malig dos seios da face",
    "C32   Neopl malig da laringe",
    "C33   Neopl malig da traqueia",
    "C34   Neopl malig dos bronquios e dos pulmoes",
    "C37   Neopl malig do timo",
    "C38   Neopl malig do coracao mediastino e pleura",
    "C39   Neop mal out loc mal def ap resp org intrat",
    "C40   Neopl malig ossos/cartilag artic membros",
    "C41   Neopl malig ossos/cartil artic outr loc e NE",
    "C43   Melanoma malig da pele",
    "C44   Outr neopl malig da pele",
    "C45   Mesotelioma",
    "C46   Sarcoma de Kaposi",
    "C47   Neopl malig nervos perif e sist nerv autonom",
    "C48   Neopl malig tec moles retro- e peritonio",
    "C49   Neopl malig tec conjuntivo e outr tec moles",
    "C50   Neopl malig da mama",
    "C51   Neopl malig da vulva",
    "C52   Neopl malig da vagina",
    "C53   Neopl malig do colo do utero",
    "C54   Neopl malig do corpo do utero",
    "C55   Neopl malig do utero porcao NE",
    "C56   Neopl malig do ovario",
    "C57   Neopl malig outr org genitais femin e NE",
    "C58   Neopl malig da placenta",
    "C60   Neopl malig do penis",
    "C61   Neopl malig da prostata",
    "C62   Neopl malig dos testiculos",
    "C63   Neopl malig outr org genit masc e NE",
    "C64   Neopl malig do rim exceto pelve renal",
    "C65   Neopl malig da pelve renal",
    "C66   Neopl malig dos ureteres",
    "C67   Neopl malig da bexiga",
    "C68   Neopl malig de outr orgaos urinarios e NE",
    "C69   Neopl malig do olho e anexos",
    "C70   Neopl malig das meninges",
    "C71   Neopl malig do encefalo",
    "C72   Neop mal med esp nerv cran out sist nerv cen",
    "C73   Neopl malig da gland tireoide",
    "C74   Neopl malig da gland supra-renal",
    "C75   Neopl malig outr gland endocrinas estr relac",
    "C76   Neopl malig outr localiz e mal definidas",
    "C77   Neopl malig secund e NE gangl linfaticos",
    "C78   Neopl malig secund org respirat e digestivos",
    "C79   Neopl malig secund de outr localiz",
    "C80   Neopl malig s/especificacao de localiz",
    "C81   Doenc de Hodgkin",
    "C82   Linfoma nao-Hodgkin folicular",
    "C83   Linfoma nao-Hodgkin difuso",
    "C84   Linfomas de celulas T cutaneas e perifericas",
    "C85   Linfoma nao-Hodgkin de outr tipos e tipo NE",
    "C88   Doenc imunoproliferativas malignas",
    "C90   Mieloma mult e neopl malig de plasmocitos",
    "C91   Leucemia linfoide",
    "C92   Leucemia mieloide",
    "C93   Leucemia monocitica",
    "C94   Outr leucemias de celulas de tipo espec",
    "C95   Leucemia de tipo celular NE",
    "C96   Outr neopl mal e NE tec linf hematop e corr",
    "C97   Neopl malig de localiz mult independentes",
    "D46   Sindr mielodisplasicas"
  )

  sim <- data.frame()
  sim$cod_mun <- as.character()
  for(c in 1:length(categorias)){
    temp <- get_sim_mun(conn = conn, ano = ano, causabas_categoria = categorias[c])
    sim <- dplyr::full_join(sim, temp, by = c("cod_mun", "cod_mun"))
  }

  sums <- rowSums(sim[,-1], na.rm = TRUE)

  sim <- data.frame(cod_mun = sim$cod_mun, sim = sums)
  sim$cod_mun <- as.character(sim$cod_mun)

  pop <- get_pop_mun(conn = conn, ano = ano)

  df <- dplyr::left_join(sim, pop, by = c("cod_mun", "cod_mun")) %>%
    mutate(indi_c.10 = sim/pop*multi) %>%
    select(cod_mun, indi_c.10)

  return(df)
}
