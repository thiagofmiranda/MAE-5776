library(tidyverse)
library(readxl)
library(irtoys)
library(naniar)


dados <- read_xlsx("/home/thiago/Downloads/BD-Hamington-0e1-FernandoPsico.xlsx",na = "NA") |> 
  setNames(c("paciente","angustia",paste0("item.",str_pad(1:13,2,"left","0")))) |> 
  mutate(item.07 = ifelse(is.na(item.07),0,item.07))

dados.dic <- dados |> 
  dplyr::select(-paciente,-angustia)

descritiva_tct_itens <- descript(dados.dic)


# Alpha de Cronbach
descritiva_tct_itens$alpha

# Correlação Ponto-Bisserial
data.frame(bisCorr=descritiva_tct_itens$bisCorr)


itens_parametros <- est(dados.dic, model="2PL", engine="ltm",nqp= 20)

itens_parametros$est; # Estimativas de (a,b)

itens_parametros$se   # Erros-padrao (standard errors)

pacientes_escores <- eap(dados.dic,itens_parametros,qu=normal.qu())

head(pacientes_escores)

hist(pacientes_escores[,1])

plot(irf(itens_parametros),label=TRUE) # Item Response Function - IRF

plot(iif(itens_parametros),label=TRUE) # Item Informations Function - IIF

plot(tif(itens_parametros),label=TRUE) # # Total Informations Function - TIF

dados_final <- tibble(paciente=dados$paciente,angustia=dados$angustia,indicador = pacientes_escores[,1]) 

dados_final[,-1] |> 
  ggplot(aes(x=indicador,y=angustia))+
  geom_point()
