# Pacotes necessarios para a aula 23/04

library(haven)
library(mlogit)
library(dplyr)
library(car)
library(nnet)
library(lmtest)
library(sandwich)
library(zoo)
library(caret)
library(DescTools)
library(gtsummary)

# Aula 23/04 - Modelo Multinomial

# Abrindo a base de dados

saneamento<-read_dta("C:/Aulas_praticas/base_demanda_san_rural.dta")

summary(saneamento)

saneamento$esg_san<- as.factor(saneamento$esg_san)

glimpse(saneamento)

# Forma de esgoto: 1=vala, rio, lago, mar, outros; 2=fossa rudimentar; 
# 3=fossa septica; 4=rede coletora

tab_esg<-table(saneamento$esg_san)
print(tab_esg)

freqrel_esg<-prop.table(tab_esg)
print(freqrel_esg)

tab_esg_mulher<-table(saneamento$esg_san, saneamento$mulher_rf)
print(tab_esg_mulher)

freqrel_esg_mulher<-prop.table(tab_esg_mulher)
print(freqrel_esg_mulher)


# Estimando o modelo logit multinomial

# categoria base esg_san = 1

saneamento$esg_san1 <- relevel(saneamento$esg_san, ref = "1")

mlogit1<-multinom(esg_san1 ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO, data = saneamento)

summary(mlogit1)

AIC(mlogit1)
BIC(mlogit1)

# odds-ratio

odds1<-exp(coef(mlogit1))
print(odds1)

# Teste de signicancia global

nullmodel<-multinom(esg_san1 ~ 1, data = saneamento)
summary(nullmodel)

anova(mlogit1, nullmodel)

PseudoR2(mlogit1, which = "McFadden")

PseudoR2(mlogit1, which = "Nagelkerke")

# significancia dos coeficientes

Anova(mlogit1, type = "II", test = "Wald")

gtsummary::tbl_regression(mlogit1, exponentiate = FALSE)
gtsummary::tbl_regression(mlogit1, exponentiate = TRUE)


# Teste Hausman-McFadden (independencia das alternativas irrelevantes)

miia<-mlogit(esg_san ~ 0 | ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO, data = saneamento, shape = "wide", reflevel = "1")

miia_2<-mlogit(esg_san ~ 0 | ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO, data = saneamento, shape = "wide", reflevel = "1", alt.subset = c("1", "3", "4"))


hmftest(miia, miia_2)

# O ideal é fazer o teste HMF com todas as especificações excluindo alguma alternativa

# matriz de confusao


conf_matrix<-confusionMatrix(predict(mlogit1), saneamento$esg_san)

print(conf_matrix)

# categoria base esg_san = 4

saneamento$esg_san4 <- relevel(saneamento$esg_san, ref = "4")

mlogit4<-multinom(esg_san4 ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO, data = saneamento)

gtsummary::tbl_regression(mlogit4, exponentiate = FALSE)
gtsummary::tbl_regression(mlogit4, exponentiate = TRUE)

conf_matrix4<-confusionMatrix(predict(mlogit4), saneamento$esg_san)

print(conf_matrix4)
