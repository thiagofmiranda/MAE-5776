library(tidyverse)
library(readxl)
library(irtoys)
library(clipr)


# Lendo os dados
dados_originais <- read_xlsx("TRI/Fernando/BD-Hamington-0e1-FernandoPsico.xlsx") 

dados <- dados_originais |> 
  setNames(c("paciente","angustia",paste0("item.",str_pad(1:13,2,"left","0"))))

# Selecionando só as colunas para a modelagerm pela tri
dados.dic <- dados |> 
  dplyr::select(-paciente,-angustia)

## ---- Análise pela teoria clássica - TCT ----
descritiva_tct_itens <- descript(dados.dic)

# Alpha de Cronbach
descritiva_tct_itens$alpha 

# Correlação Ponto-Bisserial
data.frame(bisCorr=descritiva_tct_itens$bisCorr,ExBisCorr=descritiva_tct_itens$ExBisCorr)

# Proporção de respostas
descritiva_tct_itens$perc


# Copiando análise pela TCT para um excel
write_clip(descritiva_tct_itens$perc,dec=",")
write_clip(descritiva_tct_itens$alpha,dec=",")
write_clip(data.frame(bisCorr=descritiva_tct_itens$bisCorr,ExBisCorr=descritiva_tct_itens$ExBisCorr),dec=",")


## ---- Análise pela TRI ----

# Calibrando os parâmetros
itens_parametros <- est(dados.dic, model="2PL", engine="ltm",nqp= 20)

itens_parametros$est; # Estimativas de (a,b)
itens_parametros$se   # Erros-padrao (standard errors)

# Estimando proficiências
pacientes_escores <- eap(dados.dic,itens_parametros,qu=normal.qu())

# Copiando análise pela TRI para um excel
write_clip(itens_parametros$est,dec=",")
write_clip(itens_parametros$se,dec=",")
write_clip(pacientes_escores,dec=",")


# ---- Análise gráfica pela TRI ---- 

histograma_escore <- data.frame(pacientes_escores) |> 
  ggplot(aes(x=est,fill=1))+
  geom_histogram(binwidth = 0.25,color="black")+
  ggtitle("Histograma dos Escores TRI")+
  ylab("Contagem")+
  xlab("Habilidade")+
  theme_minimal(base_size = 16)+
  theme(plot.title = element_text(size=18,hjust = 0.5),legend.position = "none")

CTT <- function(ltm_pars,filter_item="none"){
  
  irf_values <- irf(ltm_pars) # Item Response Function - IRF
  iif_values <- iif(ltm_pars) # Item Information Function - IIF
  
  irf_values_f <- bind_cols(x=irf_values$x,irf_values$f) |> 
    gather("item","irf",-x) |> 
    dplyr::select(item,x,irf)
  
  iif_values_f <- bind_cols(x=irf_values$x,iif_values$f) |> 
    gather("item","iif",-x) |> 
    dplyr::select(item,x,iif)
  
  values <- left_join(irf_values_f,iif_values_f,by=c("item","x"))
  
  if(filter_item=="none"){
    p1 <- values |>  
      ggplot(aes(x=x,color=item,group=item))+
      geom_line(aes(y=irf),size=1)+
      guides(color=guide_legend(title=NULL))+
      ylim(0,1)+
      ggtitle("Curva Característica dos Itens")+
      ylab("Probabilidade de acertar o item")+
      xlab("Habilidade")+
      theme_minimal(base_size = 16)+
      theme(plot.title = element_text(size=18,hjust = 0.5))
    
    p2 <- values |>  
      ggplot(aes(x=x,color=item,group=item))+
      geom_line(aes(y=iif),size=1)+
      guides(color=guide_legend(title=NULL))+
      ggtitle("Função de Informação dos Itens")+
      ylab("Informação do Item")+
      xlab("Habilidade")+
      theme_minimal(base_size = 16)+
      theme(plot.title = element_text(size=18,hjust = 0.5))
    
    p3 <- values |>
      group_by(x) |> 
      summarise(iif=sum(iif)) |> 
      ggplot(aes(x=x))+
      geom_line(aes(y=iif),size=1)+
      ggtitle("Função de Informação Total do Teste")+
      ylab("Informação do Teste")+
      xlab("Habilidade")+
      theme_minimal(base_size = 16)+
      theme(plot.title = element_text(size=18,hjust = 0.5))
    
    result <- ggpubr::ggarrange(p3,p1,p2,ncol=1,common.legend = TRUE,legend = "bottom")
  }else{
    
    pars <- ltm_pars$est |> 
      data.frame() |> 
      rownames_to_column("item") |> 
      setNames(c("item","a","b","c")) |> 
      filter(item==filter_item)
    
    result <- values |>  
      filter(item==filter_item) |> 
      mutate(iif=iif/4) |> 
      ggplot(aes(x=x,y=irf))+
      geom_line(color = "#E60278",size=1)+
      geom_line(aes(y=iif),color = "black",size=1)+
      scale_y_continuous("Probabilidade de acertar o Item", sec.axis = sec_axis(~ . * 4, name = "Informação do Item"))+
      ggtitle(paste0(filter_item,": a=",pars$a,"; b=",pars$b,"; c=",pars$c))+
      xlab("Habilidade")+
      theme_minimal(base_size = 16)+
      theme(plot.title = element_text(size=18,hjust = 0.5))
  }
  
  return(result)
}

plot_geral <- CTT(itens_parametros)

plot_item_01 <- CTT(itens_parametros,"item.01")
plot_item_02 <- CTT(itens_parametros,"item.02")
plot_item_03 <- CTT(itens_parametros,"item.03")
plot_item_04 <- CTT(itens_parametros,"item.04")
plot_item_05 <- CTT(itens_parametros,"item.05")
plot_item_06 <- CTT(itens_parametros,"item.06")
plot_item_07 <- CTT(itens_parametros,"item.07")
plot_item_08 <- CTT(itens_parametros,"item.08")
plot_item_09 <- CTT(itens_parametros,"item.09")
plot_item_10 <- CTT(itens_parametros,"item.10")
plot_item_11 <- CTT(itens_parametros,"item.11")
plot_item_12 <- CTT(itens_parametros,"item.12")
plot_item_13 <- CTT(itens_parametros,"item.13")

# Salvando Plots
ggsave("TRI/Fernando/plots/histograma_escore.png",histograma_escore,device = "png",width = 10,height = 8,dpi = 300,bg = "white")

ggsave("TRI/Fernando/plots/tri_plot_geral.png",plot_geral,device = "png",width = 8,height = 18,dpi = 300,bg = "white")

ggsave("TRI/Fernando/plots/tri_plot_item.01.png",plot_item_01,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.02.png",plot_item_02,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.03.png",plot_item_03,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.04.png",plot_item_04,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.05.png",plot_item_05,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.06.png",plot_item_06,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.07.png",plot_item_07,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.08.png",plot_item_08,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.09.png",plot_item_09,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.10.png",plot_item_10,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.11.png",plot_item_11,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.12.png",plot_item_12,device = "png",width = 10,height = 8,dpi = 300,bg = "white")
ggsave("TRI/Fernando/plots/tri_plot_item.13.png",plot_item_13,device = "png",width = 10,height = 8,dpi = 300,bg = "white")


# Agregando escores pela TRI com os dados originais
dados_final <- tibble(dados_originais,`Escore TRI` = pacientes_escores[,1]) 

write_clip(dados_final,dec=",")

