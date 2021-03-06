library(tidyverse)
library(ggpubr)
library(readxl)
library(irtoys)
library(naniar)
library(geobr)


# Lendo toda a base da Munic
df <- read_xlsx("TRI/Base_MUNIC_2020.xlsx",sheet=6)

# Filtro inicial
df_2 <- df |> 
  filter(!Mmam01 %in% c("Não informou","Recusa","Não informou")) |> 
  replace_with_na_all(condition = ~.x %in% c("-","Sem titular"))


dicionario <- tribble(
  ~secao.id,  ~secao.descricao,  ~item.id,     ~item.descricao, ~dicotomico,
  "5.1", "Órgão gestor do meio ambiente", "CodMun",     "Código do município",            "Não",  
  "5.1", "Órgão gestor do meio ambiente", "Mun",        "Município",                      "Não", 
  "5.1", "Órgão gestor do meio ambiente", "UF",         "Unidade da Federação",           "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Cod UF",     "Código da Unidade da Federação", "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Faixa pop",  "Faixa Populacional",             "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Regiao",     "Região territorial",             "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam01",     "Caracterização do órgão gestor",                      "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam03",     "Sexo do(a) titular do órgão gestor",                  "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam04",     "Idade do(a) titular do órgão gestor",                 "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam05",     "Cor/raça do(a) titular do órgão gestor",              "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam051",    "Foi respondido pelo próprio titular do órgão gestor", "Não",         
  "5.1", "Órgão gestor do meio ambiente", "Mmam06",     "Escolaridade do(a) titular do órgão gestor",          "Não", 
  "5.2","Capacitação","Mmam08",     "Nos últimos quatro anos servidores do município participaram de capacitação promovida pelo governo federal na área de meio ambiente",              "Sim",         
  "5.2","Capacitação","Mmam091",    "Tipo de capacitação: Estruturação da gestão municipal de meio ambiente",           "Sim",         
  "5.2","Capacitação","Mmam092",    "Tipo de capacitação: Licenciamento",                                               "Sim",         
  "5.2","Capacitação","Mmam093",    "Tipo de capacitação: Educação ambiental",                                          "Sim",         
  "5.2","Capacitação","Mmam094",    "Tipo de capacitação: Educação ambiental voltada para a agricultura familiar",      "Sim",         
  "5.2","Capacitação","Mmam095",    "Tipo de capacitação: Cadastro Ambiental Rural",                                    "Sim",         
  "5.2","Capacitação","Mmam096",    "Tipo de capacitação: Resíduos Sólidos",                                            "Sim",         
  "5.2","Capacitação","Mmam097",    "Tipo de capacitação: Produção e consumo sustentáveis",                             "Sim",         
  "5.2","Capacitação","Mmam098",    "Tipo de capacitação: Mudança do clima",                                            "Sim",         
  "5.2","Capacitação","Mmam099",    "Tipo de capacitação: Recursos Hídricos",                                           "Sim",         
  "5.2","Capacitação","Mmam0910",   "Tipo de capacitação: Participação social em fóruns e colegiados de meio ambiente", "Sim",         
  "5.2","Capacitação","Mmam0911",   "Tipo de capacitação: Outras",                                                      "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam10",     "Existência",                                                "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam101b",   "Ano de criação",                                            "Não",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam102",    "Formação do conselho",                                      "Não",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam111",    "Caráter do conselho: Consultivo",                           "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam112",    "Caráter do conselho: Deliberativo",                         "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam113",    "Caráter do conselho: Normativo",                            "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam114",    "Caráter do conselho: Fiscalizador",                         "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam12",     "Quantidade de reuniões realizadas nos últimos 12 meses",    "Não",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam13",     "Número de conselheiros (titulares e suplentes)",            "Não",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam141",    "Capacitação dos membros: Periodicamente",                   "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam142",    "Capacitação dos membros: Ocasionalmente",                   "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam143",    "Capacitação dos membros: Não realiza",                      "Sim", 
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam15",     "Município disponibiliza infraestrutura",                    "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1511",   "Município disponibiliza infraestrutura: Sala",              "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1512",   "Município disponibiliza infraestrutura: Computador",        "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1513",   "Município disponibiliza infraestrutura: Impressora",        "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1514",   "Município disponibiliza infraestrutura: Acesso à internet", "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1515",   "Município disponibiliza infraestrutura: Veículo",           "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1516",   "Município disponibiliza infraestrutura: Telefone",          "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1517",   "Município disponibiliza infraestrutura: Diárias",           "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam1518",   "Município disponibiliza infraestrutura: Dotação orçamentária própria",                                                                         "Sim", 
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam16",     "A área responsável pelo tema meio ambiente dispõe de recursos financeiros específicos para serem utilizados no desenvolvimento de suas ações", "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam17",     "O município possui Fundo Municipal de Meio Ambiente ou similar",                                                                               "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam171",    "O conselho gestor do Fundo é o Conselho Municipal de Meio Ambiente ou similar",                                                                "Sim",         
  "5.3","Conselho e Fundo municipal de meio ambiente","Mmam18",     "No ano de 2019 foi utilizado recurso do Fundo Municipal de Meio Ambiente para ações ambientais",                                               "Sim", 
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam201",    "Sobre coleta seletiva de resíduos sólidos domésticos",            "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam20011",  "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam202",    "Sobre saneamento básico",                                         "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2021",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam203",    "Sobre gestão de bacias hidrográficas",                            "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2031",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam204",    "Sobre área e/ou zona de proteção ou controle ambiental",          "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2041",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam205",    "Sobre destino das embalagens utilizadas em produtos agrotóxicos", "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2051",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam206",    "Sobre poluição do ar",                                            "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2061",   "Ano de criação",                                                  "Não", 
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam207",    "Sobre permissão de atividades extrativas minerais",               "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2071",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam208",    "Sobre fauna silvestre",                                           "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2081",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam209",    "Sobre florestas",                                                 "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2091",   "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2010",   "Sobre proteção à biodiversidade",                                 "Sim",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam20101",  "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2011",   "Sobre adaptação e mitigação de mudança do clima",                 "Sim", 
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam20111",  "Ano de criação",                                                  "Não",         
  "5.4","Legislação ou instrumento de gestão ambiental","Mmam2012",   "Nenhuma legislação citada",                                       "Sim",
  "5.6","Plano de Gestão Integrada de Resíduos Sólidos","Mmam21",     "O município possui Plano de Gestão Integrada de Resíduos Sólidos, nos termos da Política Nacional de Resíduos Sólidos", "Sim",         
  "5.6","Plano de Gestão Integrada de Resíduos Sólidos","Mmam211",    "Esse plano abrange apenas esse município",                                                                              "Sim",
  "5.7","Programas em parceria com o Governo Federal","Mmam221",    "Coletivo Educador",   "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam222",    "Sala verde",          "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam223",    "Circuito Tela Verde", "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam224",    "Etapa municipal da Conferência Infanto-Juvenil pelo Meio Ambiente",                                            "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam225",    "Educação ambiental no Plano de Gestão Integrada de Resíduos Sólidos – PGIRS",                                  "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam226",    "Sustentabilidade ambiental das instituições públicas, como a Agenda Ambiental na Administração Pública - A3P", "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam227",    "Programa de Educação Ambiental e Agricultura Familiar – PEAAF",                                                "Sim", 
  "5.7","Programas em parceria com o Governo Federal","Mmam228",    "Etapa municipal da Conferência Nacional de Meio Ambiente",                                                     "Sim",         
  "5.7","Programas em parceria com o Governo Federal","Mmam229",    "Nenhum dos programas",                                                                                         "Sim",
  "5.8","Pagamento de Serviços Ambientais","Mmam23",     "O município paga diretamente por serviços ambientais – PSA",  "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2311",   "Fonte de recursos nos últimos 12 meses: Orçamento municipal", "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2312",   "Fonte de recursos nos últimos 12 meses: Governo Federal",     "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2313",   "Fonte de recursos nos últimos 12 meses: Governo Estadual",    "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2314",   "Fonte de recursos nos últimos 12 meses: Iniciativa privada",  "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2315",   "Fonte de recursos nos últimos 12 meses: ONG",                 "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam2316",   "Fonte de recursos nos últimos 12 meses: Doações",             "Sim", 
  "5.8","Pagamento de Serviços Ambientais","Mmam2317",   "Fonte de recursos nos últimos 12 meses: Outros",              "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam241",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas que promovam a conservação e a recuperação ou melhoramento da quantidade e da qualidade dos recursos hídricos",  "Sim",
  "5.8","Pagamento de Serviços Ambientais","Mmam242",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas de conservação e preservação da vegetação nativa e da vida silvestre",                                           "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam243",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas que promovam a conservação, a recuperação ou preservação do ambiente natural nas áreas de Unidades de Conservação, em suas respectivas zonas de amortecimento e nas Terras Indígenas", "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam244",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas de recuperação e conservação dos solos e recomposição da cobertura vegetal e de áreas degradadas, através do plantio de espécies nativas em sistema agroflorestal",                    "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam245",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas de conservação de remanescentes da vegetação em áreas urbanas, de importância para a manutenção e melhoramento da qualidade do ar, dos recursos hídricos e da qualidade de vida da população", "Sim",         
  "5.8","Pagamento de Serviços Ambientais","Mmam246",    "Serviço(s) ambiental(is) abrangido(s): Pagamento por ações/iniciativas que visem especificamente a captura e retenção de carbono, com objetivo de mitigação das mudanças climáticas (conservação/restauração de ecossistemas naturais, recuperação de áreas degradadas, e adoção de práticas de manejo", "Sim",       
  "5.8","Pagamento de Serviços Ambientais","Mmam247",    "Serviço(s) ambiental(is) abrangido(s): Outros",              "Sim", 
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam25",     "Observação no município da ocorrência de algum impacto ambiental e/ou processo/ação que resulte em impacto no ambiente nos últimos 24 meses", "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam261",    "Condições climáticas extremas (secas, enxurradas)",                     "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam262",    "Poluição do ar",                                                        "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam263",    "Poluição de algum corpo d’água",                                        "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam264",    "Assoreamento de algum corpo d’água",                                    "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam265",    "Diminuição de vazão de algum corpo d’água",                             "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam266",    "Desmatamentos",                                                         "Sim", 
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam267",    "Queimadas",                                                             "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam268",    "Contaminação do solo (por agrotóxicos, fertilizantes)",                 "Sim",
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam269",    "Perda de solos por erosão e/ou desertificação (voçorocas, arenização)", "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam2610",   "Degradação de áreas legalmente protegidas",                             "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam2611",   "Diminuição da biodiversidade (fauna e flora)",                          "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam2612",   "Existência de moradia em situação de risco ambiental",                  "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam2613",   "Falta de saneamento (destinação inadequada do esgoto doméstico)",       "Sim",         
  "5.9","Impacto Ambiental e/ou processo/ação que resulte em impacto no ambiente","Mmam2614",   "Outros",                                                                "Sim")  





dic <- function(data,na0=F){
  
  data[data=="Sim"] <- "1"
  data[data=="Não"] <- "0"
  data[data=="Não informou"] <- "0"
  data[data=="Não foi instalado ou está inativo"] <- "0"
  if(na0){data[is.na(data)] <- "0"}else{data[is.na(data)] <- NA}
  
  data[data!="1" & data!="0"] <- NA
  
  data
}

####################################################################################################
dic_vars <- pull(filter(dicionario,dicotomico=="Sim"|secao.id=="5.1"),item.id)

no_vars <- c("Mmam08","Mmam0911","Mmam10", "Mmam141","Mmam142","Mmam143","Mmam15","Mmam171","Mmam2012","Mmam211","Mmam229","Mmam2614",
             "Mmam2311","Mmam2312","Mmam2313","Mmam2314","Mmam2315","Mmam2316","Mmam2317","Mmam241","Mmam242","Mmam243","Mmam244","Mmam245","Mmam246","Mmam247")

no_mun <- c(2918506, 2101350, 2410405, 3540309, 5208400, 1303569, 2111763, 
            2307502, 2308807, 2609154, 2708501, 5103437, 5208400,
            1507102, 2101350, 2102408, 2111763, 2308807, 2708501,
            2101350, 2103000, 2707305, 2708501, 2918506)

df_final <- df_2 |> 
  dplyr::select(any_of(c(dic_vars,"Faixa_pop"))) |> 
  dplyr::select(-any_of(no_vars)) |> 
  filter(!CodMun %in% no_mun)

df_info <- df_final |> 
  dplyr::select(CodMun,Mun,`Cod UF`,UF,Faixa_pop,Regiao,Mmam01,Mmam03,Mmam04,Mmam05,Mmam051,Mmam06) 

df_dic <- df_final|> 
  dplyr::select(-CodMun,-Mun,-`Cod UF`,-UF,-Faixa_pop,-Regiao,-Mmam01,-Mmam03,-Mmam04,-Mmam05,-Mmam051,-Mmam06) |> 
  dic(na0 = T) |> 
  mutate_all(as.numeric) 

df_info <- df_info[rowSums(df_dic) >= 5,]
df_dic <- df_dic[rowSums(df_dic) >= 5,]

df_final |> nrow()
df_dic |> nrow()

# Análises pela TCT
tct <- descript(df_dic) # 1553
#tct <- reliability(df_dic) # 1553


# Selecionando itens com boa correlação
itens_1 <- data.frame(bisCorr=tct$bisCorr,ExBisCorr=tct$ExBisCorr) |> 
  #filter(ExBisCorr>=0.3) |> 
  row.names() 

df_dic_2 <- df_dic |> 
  dplyr::select(all_of(itens_1))

itens_selecionados <- dicionario |> 
  filter(item.id %in% colnames(df_dic_2))

#tct_2 <- descript(df_dic_2)

itens_parametros_1 <- est(df_dic_2, model="2PL", engine="ltm",nqp= 20)

exc_itens_tri_1 <- itens_parametros_1$est |> 
  data.frame() |> 
  filter(X2 > 5 | X2 < -5 | X1 > 4) |> 
  rownames()

df_dic_3 <- df_dic_2 |> 
  dplyr::select(-any_of(exc_itens_tri_1)) 

itens_parametros_2 <- est(df_dic_3, model="2PL", engine="ltm",nqp= 20)
exc_itens_tri_2 <- itens_parametros_2$est |> 
  data.frame() |> 
  filter(X2 > 5 | X2 < -5 | X1 > 4) |> 
  rownames()


df_dic_4 <- df_dic_3 |> 
  dplyr::select(-any_of(exc_itens_tri_2)) 

itens_parametros_3 <- est(df_dic_4, model="2PL", engine="ltm",nqp= 20)
itens_parametros_3$est

escores <- eap(df_dic_4,itens_parametros_3,qu=normal.qu())

gghistogram(escores[,1])
ggqqplot(escores[,1])
#shapiro.test(escores[,1])

library(geobr)
options(timeout= 4000000)
metadata <- download_metadata() # para ver codigos
head(metadata)

mun <- read_municipality(year = 2020)

df_escore <- df_info |> 
  bind_cols(data.frame(escores)) |> 
  dplyr::select(CodMun,est)

dataset_final = left_join(mun, df_escore, by=c("code_muni"="CodMun")) 

plot_geo <- ggplot() +
  geom_sf(data=dataset_final, aes(fill=est), color= NA, size=.15)+
  scale_fill_distiller(palette = "Greens", limits=c(-2.5, 3.5),direction = 1,
                       name="Indicador")+
  theme_minimal(base_size = 16)

save.image("TRI/data.RData")