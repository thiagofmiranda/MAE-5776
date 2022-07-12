

cov_pop <- matrix(c(5,3,3,5),2,2)
mean_pop <- c(30, 20)

set.seed(123)
pop <- MASS::mvrnorm(100,mean_pop,cov_pop) 

n <- 100
p <- 2

mi <- colMeans(pop)
s <- cov(pop)
mu0 <- mean_pop



#Caso 1: Supondo Uma única População e p=2 variáveis

# um unico teste para o vetor de todas as p variáveis:
# Vetor centróide vs vetor centróide de referência 

T2 <- n%*%t(mi-mu0)%*%solve(s)%*%(mi-mu0)
T2

T2.critico<-(((n-1)*p)/(n-p))*qf(0.95,p,(n-p),lower.tail = TRUE) # --> c²

T2 <= T2.critico 


# R(mi|Y) <=c²

# Interpretação:
# 0 <= c² <= T²: Rejeitamos H0 (Fora da região de aceitação)
# 0 <= T² <= c²: Não rejeitamos H0 (Dentro da região de aceitação)

# Como 
T2 <= T2.critico 
# é verdadeiro, ou seja, a estatística T² está dentro da regição de aceitação,
# não rejeitamos a hipótese de que mu0 representa a média (centróide)

# ----------------------------------------------------

#Estatísticas "t" ao quadrado univariadas para cada uma das 2 variáveis
t21 <- n*(mi[1]-mu0[1])^2/s[1,1]
t22 <- n*(mi[2]-mu0[2])^2/s[2,2]
t21
t22

#comparar com a estatística F (valor crítico)
t21 <= qf(0.95,1,(n-1),lower.tail = TRUE)  
t22 <= qf(0.95,1,(n-1),lower.tail = TRUE)  

#Conclusão: Não há evidência de rejeição em nenhuma variável
# as quais não são significantemente diferente da média de referência



#Obter os IC95% para cada Variável
t1_classico <- t.test(pop[,1],conf.level=0.95) 
t2_classico <- t.test(pop[,2],conf.level=0.95) 

#Obter os IC95% para cada Variável com correção de bomferroni -> IC97.5%
t1_classico_bonf <- t.test(pop[,1],conf.level=0.975) 
t2_classico_bonf <- t.test(pop[,2],conf.level=0.975) 

#Intervalos de Confiança Simultâneos
e1 <- sqrt((s[1,1]/n)*((n-1)*p/(n-p))*qf(0.95,p,n-p))
ICS1i.C1<- mi[1]-e1
ICS1s.C1<- mi[1]+e1

e2 <- sqrt((s[2,2]/n)*((n-1)*p/(n-p))*qf(0.95,p,n-p))
ICS1i.P1<- mi[2]-e2
ICS1s.P1<- mi[2]+e2

#Intervalos de Confiança Simultâneos com correção de bomferroni -> IC97.5%
e1 <- sqrt((s[1,1]/n)*((n-1)*p/(n-p))*qf(0.975,p,n-p))
ICSB1i.C1<- mi[1]-e1
ICSB1s.C1<- mi[1]+e1

e2 <- sqrt((s[2,2]/n)*((n-1)*p/(n-p))*qf(0.975,p,n-p))
ICSB1i.P1<- mi[2]-e2
ICSB1s.P1<- mi[2]+e2


# Obter os p-valores individuais e corrigir para múltiplos testes
t1 <- (mi[1]-mu0[1])/sqrt(s[1,1]/n)
t2 <- (mi[2]-mu0[2])/sqrt(s[2,2]/n)

pt1<- pt(c(abs(t1)), df=n-1, lower.tail=FALSE)
pt2<- pt(c(abs(t2)), df=n-1, lower.tail=FALSE)

results <- 2*c(pt1,pt2)
results  

p.adjust.methods
padjustB <- p.adjust(results,method="bonferroni")

cbind(results,padjustB,padjustFDR,padjustHOLM)




# Ellipse radius from Chi-Sqaure distrubiton
rad  = qchisq(p = 0.05 , df = ncol(pop))
# Square root of Chi-Square value
rad  = sqrt(rad)
# Finding ellipse coordiantes
ellipse <- car::ellipse(center = mi , shape = s , radius = rad , segments = 150 , draw = FALSE)


colnames(pop) <- c("X", "Y")
colnames(ellipse) <- c("X", "Y")


plot <- data.frame(X = t1_classico$conf.int[1:2], Y = t2_classico$conf.int[1:2]) |> 
  ggplot(aes(x = X , y = Y)) +
  geom_polygon(data = data.frame(ellipse) , fill = "red" , alpha = 0.2)+
  geom_rect(data=data.frame(X = c(ICSB1i.C1,ICSB1s.C1), Y = c(ICSB1i.P1,ICSB1s.P1)),
            aes(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y)), fill = "blue", alpha = 0.2)+
  
  geom_text(data=data.frame(X = c(ICSB1i.C1,ICSB1s.C1), Y = c(ICSB1i.P1,ICSB1s.P1)),
            aes(label = c("ICSBonf-","ICSBonf+")) , size = 5 )+
  
  geom_rect(aes(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y)), fill = "blue", alpha = 0.2)+
  
  geom_rect(data=data.frame(X = c(ICS1i.C1,ICS1s.C1), Y = c(ICS1i.P1,ICS1s.P1)),
            aes(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y)), fill = "yellow", alpha = 0.2)+
  geom_rect(data=data.frame(X = t1_classico_bonf$conf.int[1:2], Y = t2_classico_bonf$conf.int[1:2]),
            aes(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y)), fill = "darkred", alpha = 0.3)+
  geom_text(data=data.frame(X = c(ICS1i.C1,ICS1s.C1), Y = c(ICS1i.P1,ICS1s.P1)),
            aes(label = c("ICS-","ICS+")) , size = 5 )+
  geom_text( aes(label = c("IC-","IC+")) , size = 5 )+ 
  geom_text(data=data.frame(X = t1_classico_bonf$conf.int[1:2], Y = t2_classico_bonf$conf.int[1:2]),
            aes(label = c("ICBonf-","ICBonf+")) ,size = 5 )+
  
  ylab("Y") + xlab("X")+
  theme_bw()

ggsave(filename = "questão15.png",plot)




