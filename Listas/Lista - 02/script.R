

library(tidyverse)
# Questão 1

media_pop_1 <- c(5.0, 5.3, 5.2)
media_pop_2 <- c(5.3, 5.1, 5.7)

cov_pop_1 <- matrix(c(1, 0.3, 0.5, 
                      0.3, 0.9, 0.2,
                      0.5, 0.2, 1.1),3,3,byrow=T)
cov_pop_2 <- matrix(c(1, 0.3, 0.5, 
                      0.3, 0.9, 0.2,
                      0.5, 0.2, 1.1),3,3,byrow=T)

cor_pop_1 <- matrix(
  c(cov_pop_2[1,1]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[1,2]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[1,3]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[3,3])),
    cov_pop_2[2,1]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[2,2]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[2,3]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[3,3])),
    cov_pop_2[3,1]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[3,2]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[3,3]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[3,3]))),
  3,3,byrow=T) 

cor_pop_2 <- matrix(
  c(cov_pop_2[1,1]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[1,2]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[1,3]/(sqrt(cov_pop_2[1,1])*sqrt(cov_pop_2[3,3])),
    cov_pop_2[2,1]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[2,2]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[2,3]/(sqrt(cov_pop_2[2,2])*sqrt(cov_pop_2[3,3])),
    cov_pop_2[3,1]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[1,1])),
    cov_pop_2[3,2]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[2,2])),
    cov_pop_2[3,3]/(sqrt(cov_pop_2[3,3])*sqrt(cov_pop_2[3,3]))),
  3,3,byrow=T)


set.seed(123)
pop_1 <- MASS::mvrnorm(100,media_pop_1,cov_pop_1) |> 
  as_tibble() |> 
  mutate(pop = "Pop 1")

pop_2 <- MASS::mvrnorm(100,media_pop_2,cov_pop_2) |> 
  as_tibble() |> 
  mutate(pop = "Pop 2")



pop <- bind_rows(pop_1, pop_2) |> 
  rename(Y1 = V1, Y2 = V2, Y3 = V3)



library(GGally)

# From the help page:
data(flea)
ggpairs(pop, ggplot2::aes(colour=pop)) 





# Questão 1.2
De um modo geral, são 2 variáveis contínuas com 200 observações e uma variável 
categórica agupando os dados. As amostras são compostas por 2 variáveis, 
onde cada população tem 100 obsevações, ou seja, estão baleceadas.
(comentar sobre independência)



# Questão 1.3
A variável Y2 contribui mais para a diferença entre as populações, 
sendo a variável que apresenta a maior diferença média, 3.5.


# Questão 1.4
pop |> 
  group_by(pop) |> 
  summarise(Y1_bar = mean(Y1),Y2_bar = mean(Y2))

cov(pop[pop$pop=="Pop 1",-3])
cov(pop[pop$pop=="Pop 2",-3])

cor(pop[pop$pop=="Pop 1",-3])
cor(pop[pop$pop=="Pop 2",-3])

library(biotools)

teste_boxM <- boxM(data = pop[,-3], grouping = pop$pop) 
teste_boxM

# Questão 1.5
Como o p-valor de 0.8825 é maior que o nível de significância de 5%. Não rejeitamos a hipótese nula de que
as matrizes de covariancias são iguais.


library(Hotelling)
teste_t2_hotelling <- hotelling.test(pop[pop$pop=="Pop 1",-3],pop[pop$pop=="Pop 2",-3], var.equal = TRUE)
teste_t2_hotelling

Como o p-valor de 0 é menor que o nível de significância de 5%. Rejeitamos a hipótese nula 
e concluímos que há algumas diferença entre as populações para o conjunto das variáveis Y1 e Y2.


# Questão 1.6
bartlett.test(pop$Y1~pop$pop) 
y1_tteste <- t.test(pop$Y1~pop$pop,conf.level=0.95,var.equal=TRUE)

Como o p-valor do teste T de 0.6919 é menor que o nível de significância de 5%,
Não rejeitamos a hipóteses nula e concluímos que as variâncias da variável Y1 
entre os grupos são homogêneas.
Como o p-valor do teste T de 0.002375 é menor que o nível de significância de 5%, 
rejeitamos a hipóteses nula e concluímos que as médias das variável Y1 
entre as amostras das populações 1 e 2 são diferentes,ao nível de 95% de confiança. 



bartlett.test(pop$Y2~pop$pop) # teste de Variâncias homogêneas para Y1
y2_tteste <- t.test(pop$Y2~pop$pop,conf.level=0.95,var.equal=TRUE)

Como o p-valor do teste T de 0.6426 é menor que o nível de significância de 5%,
Não rejeitamos a hipóteses nula e concluímos que as variâncias da variável Y1 
entre os grupos são homogêneas.
Como o p-valor do teste T de 2.2e-16 é menor que o nível de significância de 5%, 
rejeitamos a hipóteses nula e concluímos que as médias das variável Y1 
entre as amostras das populações 1 e 2 são diferentes,ao nível de 95% de confiança.


# Questão 1.6

#Obter os p-valores (bicaudal) individuais com correção para múltiplos testes
p.result <- c(y1_tteste$p.value,y2_tteste$p.value)
p.result  

p.adjust.methods
padjustB <- p.adjust(p.result,method="bonferroni")
padjustFDR <- p.adjust(p.result,method="fdr")
padjustHOLM <- p.adjust(p.result,method="holm")

cbind(p.result,padjustB,padjustFDR,padjustHOLM) |> round(4)



# -----------------------------------------------------------------------------------
# Questão 2

#Dados de duas espécies de moscas, 
#Leptoconops torrens (=0) e Leptoconops carteri (=1),
#apresentados em Johnson and Wichern (1992)
mosc <- tribble(
  ~sp, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7,
  0, 85, 41, 31, 13, 25, 9, 8,
  0, 87, 38, 32, 14, 22, 13, 13,
  0, 94, 44, 36, 15, 27, 8, 9,
  0, 92, 43, 32, 17, 28, 9, 9,
  0, 96, 43, 35, 14, 26, 10, 10,
  0, 91, 44, 36, 12, 24, 9, 9,
  0, 90, 42, 36, 16, 26, 9, 9,
  0, 92, 43, 36, 17, 26, 9, 9,
  0, 91, 41, 36, 14, 23, 9, 9,
  0, 87, 38, 35, 11, 24, 9, 10,
  0, 97, 45, 39, 17, 27, 9, 10,
  0, 89, 38, 36, 13, 22, 9, 9,
  0, 94, 45, 37, 13, 26, 9, 9,
  0, 96, 44, 37, 14, 24, 9, 10,
  0, 104, 49, 35, 14, 21, 10, 10,
  0, 94, 41, 31, 17, 26, 10, 9,
  0, 99, 44, 31, 18, 28, 10, 9,
  0, 94, 38, 32, 13, 22, 9, 9,
  0, 94, 43, 37, 16, 26, 9, 10,
  0, 93, 43, 38, 14, 28, 10, 10,
  0, 95, 44, 37, 18, 27, 10, 10,
  0, 95, 45, 39, 13, 27, 10, 10,
  0, 96, 39, 37, 12, 26, 8, 8,
  0, 103, 46, 34, 18, 26, 10, 10,
  0, 108, 44, 37, 14, 25, 11, 11,
  0, 106, 47, 38, 15, 26, 10, 10,
  0, 105, 46, 34, 14, 31, 10, 11,
  0, 103, 44, 34, 15, 23, 10, 10,
  0, 100, 41, 35, 14, 24, 10, 10,
  0, 109, 44, 36, 13, 27, 11, 10,
  0, 104, 45, 36, 15, 30, 10, 10,
  0, 95, 40,  35, 14, 23, 9, 10,
  0, 104, 44, 34, 15, 29, 9, 10,
  0, 90, 40,  37, 12, 22, 9, 10,
  0, 104, 46, 37, 14, 30, 10, 10,
  1, 86, 19,  37, 11, 25, 9, 9,
  1, 94, 40,  38, 14, 31, 6, 7,
  1, 103, 48, 39, 14, 33, 10, 10,
  1, 82, 41,  35, 12, 25, 9, 8,
  1, 103, 43, 42, 15, 32, 9, 9,
  1, 101, 43, 40, 15, 25, 9, 9,
  1, 103, 45, 44, 14, 29, 11, 11,
  1, 100, 43, 40, 18, 31, 11, 10,
  1, 99, 41,  42, 15, 31, 10, 10,
  1, 100, 44, 43, 16, 34, 10, 10,
  1, 112, 47, 44, 16, 38, 12, 11,
  1, 99, 48,  37, 14, 32, 10, 9,
  1, 98, 45,  41, 19, 31, 9, 8,
  1, 101, 46, 42, 14, 24, 11, 10,
  1, 99, 45,  37, 13, 28, 10, 9,
  1, 103, 47, 44, 15, 20, 8, 9,
  1, 98, 40,  38, 12, 32, 9, 8,
  1, 101, 46, 36, 14, 28, 10, 10,
  1, 101, 46, 40, 17, 32, 9, 9,
  1, 98, 47,  39, 15, 33, 10, 10,
  1, 99, 45,  42, 15, 32, 10, 9,
  1, 102, 45, 44, 15, 30, 10, 10,
  1, 97, 45,  37, 15, 32, 10, 9,
  1, 96, 39,  40, 14, 20, 9, 9,
  1, 89, 39,  33, 12, 20, 9, 8,
  1, 99, 42,  38, 14, 33, 9, 9,
  1, 110, 45, 41, 17, 36, 9, 10,
  1, 99, 44,  35, 16, 31, 10, 10,
  1, 103, 43, 38, 14, 32, 10, 10,
  1, 95, 46,  36, 15, 31, 8, 8,
  1, 101, 47, 38, 14, 37, 11, 11,
  1, 103, 47, 40, 15, 32, 11, 11,
  1, 99, 43,  37, 14, 23, 11, 10,
  1, 105, 50, 40, 16, 33, 12, 11,
  1,  99, 47, 39, 14, 34, 7, 7)


head(mosc)

# Questão 2.1

# Questão 2.2
v1_bartlett_teste <- bartlett.test(mosc$V1~mosc$sp) # Homogênea
v2_bartlett_teste <- bartlett.test(mosc$V2~mosc$sp) # Heterogênea
v3_bartlett_teste <- bartlett.test(mosc$V3~mosc$sp) # Homogênea
v4_bartlett_teste <- bartlett.test(mosc$V4~mosc$sp) # Homogênea
v5_bartlett_teste <- bartlett.test(mosc$V5~mosc$sp) # Heterogênea
v6_bartlett_teste <- bartlett.test(mosc$V6~mosc$sp) # Homogênea
v7_bartlett_teste <- bartlett.test(mosc$V7~mosc$sp) # Homogênea

v1_bartlett_teste$p.value
v2_bartlett_teste$p.value
v3_bartlett_teste$p.value
v4_bartlett_teste$p.value
v5_bartlett_teste$p.value
v6_bartlett_teste$p.value
v7_bartlett_teste$p.value

v1_tteste <- t.test(mosc$V1~mosc$sp, conf.level=0.95, var.equal=TRUE)
v2_tteste <- t.test(mosc$V2~mosc$sp, conf.level=0.95, var.equal=FALSE)
v3_tteste <- t.test(mosc$V3~mosc$sp, conf.level=0.95, var.equal=TRUE)
v4_tteste <- t.test(mosc$V4~mosc$sp, conf.level=0.95, var.equal=TRUE)
v5_tteste <- t.test(mosc$V5~mosc$sp, conf.level=0.95, var.equal=FALSE)
v6_tteste <- t.test(mosc$V6~mosc$sp, conf.level=0.95, var.equal=TRUE)
v7_tteste <- t.test(mosc$V7~mosc$sp, conf.level=0.95, var.equal=TRUE)

v1_p_tteste <- v1_tteste$p.value
v2_p_tteste <- v2_tteste$p.value
v3_p_tteste <- v3_tteste$p.value
v4_p_tteste <- v4_tteste$p.value
v5_p_tteste <- v5_tteste$p.value
v6_p_tteste <- v6_tteste$p.value
v7_p_tteste <- v7_tteste$p.value

v1_tteste$conf.int
v2_tteste$conf.int
v3_tteste$conf.int
v4_tteste$conf.int
v5_tteste$conf.int
v6_tteste$conf.int
v7_tteste$conf.int


# Testar a Homocedasticidade das Matrizes de Covariância

teste_boxM <- boxM(data = mosc[,-1], grouping = mosc$sp) 
teste_boxM$p.value

Há evidência amostral de que as matrizes de covariâncias são heterogêneas

teste_t2_hotelling <- hotelling.test(mosc[mosc$sp==0,-1],mosc[mosc$sp==1,-1], var.equal = FALSE)
teste_t2_hotelling$pval

Há evidência para rejeitar a hip de centróides dos grupos iguais


cm.pooled <- teste_boxM$pooled
n1 <- nrow(mosc[mosc$sp==0,])
n2 <- nrow(mosc[mosc$sp==1,])

p <- ncol(mosc)-1

d.barra<-mu0-mu1


mu0 <- colMeans(mosc[mosc$sp==0,-1]) #médias do grupo 0
mu1 <- colMeans(mosc[mosc$sp==1,-1]) #médias do grupo 1

s0 <- cov(mosc[mosc$sp==0,-1]) #matriz de cov do grupo 0
s1 <- cov(mosc[mosc$sp==1,-1]) #matriz de cov do grupo 1

errors <- c(
  sqrt((((n1+n2-2)*p)/(n1+n2-p-1))*qf(0.95,p,n1+n2-p-1))*sqrt((1/n1+1/n2)*(cm.pooled[1,1])), # Homogênea
  sqrt(qchisq(0.95,p))*sqrt((s0[2,2]/n1)+(s1[2,2]/n2)), # Heterogênea
  sqrt((((n1+n2-2)*p)/(n1+n2-p-1))*qf(0.95,p,n1+n2-p-1))*sqrt((1/n1+1/n2)*(cm.pooled[3,3])), # Homogênea
  sqrt((((n1+n2-2)*p)/(n1+n2-p-1))*qf(0.95,p,n1+n2-p-1))*sqrt((1/n1+1/n2)*(cm.pooled[4,4])), # Homogênea
  sqrt(qchisq(0.95,p))*sqrt((s0[5,5]/n1)+(s1[5,5]/n2)), # Heterogênea
  sqrt((((n1+n2-2)*p)/(n1+n2-p-1))*qf(0.95,p,n1+n2-p-1))*sqrt((1/n1+1/n2)*(cm.pooled[6,6])), # Homogênea
  sqrt((((n1+n2-2)*p)/(n1+n2-p-1))*qf(0.95,p,n1+n2-p-1))*sqrt((1/n1+1/n2)*(cm.pooled[7,7])) # Homogênea
)

tibble(
  var = paste0("V", 1:7),
  d.barra = d.barra,
  ICS.neg = d.barra-errors,
  ICS.pos = d.barra+errors
  )


# Questão 2.3

#Obter os p-valores (bicaudal) individuais com correção para múltiplos testes
p.result <- c(v1_p_tteste,
              v2_p_tteste,
              v3_p_tteste,
              v4_p_tteste,
              v5_p_tteste,
              v6_p_tteste,
              v7_p_tteste)
p.result  

p.adjust.methods
padjustB <- p.adjust(p.result,method="bonferroni")
padjustFDR <- p.adjust(p.result,method="fdr")
padjustHOLM <- p.adjust(p.result,method="holm")

cbind(p.result,padjustB,padjustFDR,padjustHOLM) |> round(4)





































