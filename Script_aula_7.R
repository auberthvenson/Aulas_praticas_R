# Pacotes necessarios para a aula 30/04

library(causaldata)
library(tidyverse)   
library(ggplot2)      
library(stargazer)    
library(haven)        
library(AER)          
library(plm)          
library(sandwich)     
library(lmtest)
library(coefplot)  
library(fixest)

# Aula 30/04 - Modelo Diff-in-Diff

# Abrindo a base de dados


organdon <- causaldata::organ_donations

summary(organdon)

# Criando a variavel tempo

organdon$time = ifelse(organdon$Quarter_Num >= 4, 1, 0)

# Criando a variavel grupo de tratamento

organdon$group = ifelse(organdon$State == "California", 1, 0)

# Criando a variavel tratado

organdon$treated = organdon$time*organdon$group

# Estimando modelo Diff-in-Diff

# Abordagem OLS

didols<-lm(Rate ~ time + group + treated, data = organdon)

summary(didols)

bptest(didols)

# erros padrão robustos (hetorecedasticidade)

robust_se <- sqrt(diag(vcovHC(didols, type = "HC1")))  
stargazer(didols, type="text", se=list(robust_se), digits = 5) 

# erro padrão robusto (cluster por estado)

cluster_se <- sqrt(diag(vcovCL(didols, cluster = ~ State)))
stargazer(didols, type="text", se=list(cluster_se), digits = 5)


# Abordagem two-way fixed effects

didfe <- feols(Rate ~ treated | State + Quarter, data = organdon)

summary(didfe)


# Testando a hipotese de tendencias paralelas

organdon$T2 = ifelse(organdon$Quarter_Num == 2, 1, 0)
organdon$T3 = ifelse(organdon$Quarter_Num == 3, 1, 0)

organdon$faketreat2 = organdon$T2*organdon$group
organdon$faketreat3 = organdon$T3*organdon$group

didfaket <- feols(Rate ~ treated + faketreat2 + faketreat3 | State + Quarter, data = organdon)

summary(didfaket)


# Os coeficientes do tratamento falso nao sao significativos, entao a hipotese
# de tendencias paralelas e valida

# Efeito de longo prazo do tratamento

efflp <- feols(Rate ~ i(Quarter_Num, group, ref = 3) |  State + Quarter_Num, data = organdon)

summary(efflp)
coefplot(efflp)

# Grafico

DiD2 <- aggregate(Rate ~ group + time, data = organdon, FUN = mean)

ggplot(DiD2, aes(x = time, y = Rate, color = group)) +
  geom_point(size = 2) +
  geom_line(aes(group = group)) +
  labs(title = "Gráfico Diff-in-Diff",
       x = "Tempo",
       y = "Resultado")

DiD6 <- aggregate(Rate ~ group + Quarter_Num, data = organdon, FUN = mean)

ggplot(DiD6, aes(x = Quarter_Num, y = Rate, color = group)) +
  geom_point(size = 2) +
  geom_line(aes(group = group)) +
  labs(title = "Gráfico Diff-in-Diff",
       x = "Tempo",
       y = "Resultado")
