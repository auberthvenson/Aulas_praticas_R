# Pacotes necessarios para a aula 30/04

library(haven)
library(censReg)
library(lmtest)
library(sandwich)
library(gmodels)

# Aula 30/04 - Modelo Tobit

# Abrindo a base de dados

traicao<-read_dta("C:/Aulas_praticas/Affairs.dta")

# Verificando concentracao no valor limite

hist(traicao$naffairs)

table(traicao$affair)
table(traicao$naffairs)

table(traicao$male)
table(traicao$naffairs, traicao$male)
table(traicao$affair, traicao$male)
CrossTable(traicao$affair, traicao$male)


# pela alta concentracao no valor zero (limite inferior) é necessario
# o modelo tobit

# Estimando o modelo tobit

tobit<-censReg(naffairs ~ age + male + yrsmarr + kids + ratemarr + relig + educ, left = 0 , right = Inf, data = traicao)

summary(tobit)

# criterios de informacao e pseudoR2

AIC(tobit)
BIC(tobit)

nullmodel<-censReg(naffairs ~ 1, left = 0 , right = Inf, data = traicao)

1 - (logLik(tobit) / logLik(nullmodel))

# Calculando os efeitos marginais

margeff<-margEff(tobit)

summary(margeff)

# erro-padrao robusto

robust_se<-vcovHAC(tobit, type = ("HC3"))
print(robust_se)

teste_robusto <- coeftest(tobit, vcov = robust_se)
print(teste_robusto)