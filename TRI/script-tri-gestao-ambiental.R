library(tidyverse)
library(readxl)
library(naniar)


df <- read_xlsx("Base_MUNIC_2020.xlsx",sheet=6)

df_final <- df |> 
  filter(!Mmam01 %in% c("Recusa","Não possui estrutura")) |> 
  replace_with_na_all(condition = ~.x %in% c("Recusa","Não informou","-","Sem titular"))

# df_5_1 <- df_final |> 
#   select(CodMun,UF,`Cod UF`,Mun,Faixa_pop,Regiao,Mmam01,Mmam03,Mmam04,Mmam05,Mmam051,Mmam06)
# df_5_2 <- df_final |> 
#   select(CodMun,Mmam08,Mmam091,Mmam092,Mmam093,Mmam094,Mmam095,Mmam096,Mmam097,Mmam098,Mmam099,Mmam0910,Mmam0911)
# df_5_3 <- df_final |> 
#   select(CodMun,Mmam10,Mmam101b,Mmam102,Mmam111,Mmam112,Mmam113,Mmam114,Mmam12,Mmam13,Mmam141,
#          Mmam142,Mmam143,Mmam15,Mmam1511,Mmam1512,Mmam1513,Mmam1514,Mmam1515,Mmam1516,Mmam1517,Mmam1518,
#          Mmam16,Mmam17,Mmam171,Mmam18)
# df_5_4 <- df_final |> 
#   select(CodMun,Mmam201,Mmam20011,Mmam202,Mmam2021,Mmam203,Mmam2031,Mmam204,Mmam2041,Mmam205,Mmam2051,
#          Mmam206,Mmam2061,Mmam207,Mmam2071,Mmam208,Mmam2081,Mmam209,Mmam2091,Mmam2010,Mmam20101,Mmam2011,
#          Mmam20111,Mmam2012)
# df_5_6 <- df_final |> 
#   select(CodMun,Mmam21,Mmam211)
# df_5_7 <- df_final |> 
#   select(CodMun,Mmam221,Mmam222,Mmam223,Mmam224,Mmam225,Mmam226,Mmam227,Mmam228,Mmam229)
# df_5_8 <- df_final |> 
#   select(CodMun,Mmam23,Mmam2311,Mmam2312,Mmam2313,Mmam2314,Mmam2315,Mmam2316,Mmam2317,
#          Mmam241,Mmam242,Mmam243,Mmam244,Mmam245,Mmam246,Mmam247)
# df_5_9 <- df_final |> 
#   select(CodMun,Mmam25,Mmam261,Mmam262,Mmam263,Mmam264,Mmam265,Mmam266,Mmam267,Mmam268,Mmam269,
#          Mmam2610,Mmam2611,Mmam2612,Mmam2613,Mmam2614)


df_5_2 <- df_final |> 
  select(CodMun,Mmam08,Mmam091,Mmam092,Mmam093,Mmam094,Mmam095,Mmam096,Mmam097,Mmam098,Mmam099,Mmam0910,Mmam0911)
df_5_2[is.na(df_5_2)] <- "Não"

df_5_3 <- df_final |> 
  select(CodMun,Mmam10,Mmam16,Mmam17)

df_5_4 <- df_final |> 
  select(CodMun,Mmam201,Mmam202,Mmam203,Mmam204,Mmam205,Mmam206,Mmam207,
         Mmam208,Mmam209,Mmam2010,Mmam2011)

df_5_6 <- df_final |> 
  select(CodMun,Mmam21)

df_5_7 <- df_final |> 
  select(CodMun,Mmam221,Mmam222,Mmam223,Mmam224,Mmam225,Mmam226,Mmam227,Mmam228)

df_5_8 <- df_final |> 
  select(CodMun,Mmam23)

df_5_9 <- df_final |> 
  select(CodMun,Mmam25,Mmam261,Mmam262,Mmam263,Mmam264,Mmam265,Mmam266,Mmam267,Mmam268,Mmam269,
         Mmam2610,Mmam2611,Mmam2612,Mmam2613,Mmam2614)
df_5_9[is.na(df_5_9)] <- "Não"


df_final <- df_5_2 |> 
  left_join(df_5_3, by = "CodMun") |> 
  left_join(df_5_4, by = "CodMun") |> 
  left_join(df_5_6, by = "CodMun") |> 
  left_join(df_5_7, by = "CodMun") |> 
  left_join(df_5_8, by = "CodMun") |> 
  left_join(df_5_9, by = "CodMun") 

df_final[df_final=="Sim"] <- "1"
df_final[df_final=="Não"] <- "0"

df_final <- df_final |> 
  mutate_all(as.numeric) |> 
  na.omit()


df_final



if(!require(irtoys)) install.packages("irtoys"); library(irtoys)

par = est(df_final[,-1], model="2PL", engine="ltm",nqp= 20)

par$est; # Estimativas de (a,b)
par$se   # Erros-padrao (standard errors)

sco = eap(df_final[,-1],par,qu=normal.qu())

head(sco)

hist(sco[,1])

plot(irf(par),label=TRUE) # Item Response Function - IRF

plot(iif(par),label=TRUE) # Item Informations Function - IIF

plot(tif(par),label=TRUE) # # Total Informations Function - TIF

res <- tibble(CodMun=df_final$CodMun,indicador = sco[,1]) |> 
  left_join(df,by="CodMun") |> 
  dplyr::select(CodMun,Mun,UF,Faixa_pop,Regiao,indicador)


res |> 
  arrange(indicador) |> 
  head(10)

res |> 
  arrange(desc(indicador)) |> 
  head(10)





