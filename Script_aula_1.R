# Pacotes necessarios para a aula 26/03

library(readxl)
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(knitr)

# Aula 26/03 - Estacionariedade

# Abrindo a base de dados

pibpc<-read_excel("C:/Aulas_praticas/pibpc.xlsx")

View(pibpc)

summary(pibpc)

# Criando a serie de tempo

ts_pibpc<-ts(pibpc [2], start = c(2000), end = c(2021), frequency = 1) 

# Gráfico temporal, FAC, FACP

plot(ts_pibpc, main= "PIB pc")

Acf(ts_pibpc, 10, main = "PIB pc")
Pacf(ts_pibpc, 10, main = "PIB pc")

# Testes de Raiz Unitaria

# H0 raiz unitaria

testeadfpibpc<-adf.test(ts_pibpc)

print(testeadfpibpc)

testepppibpc<-pp.test(ts_pibpc)

print(testepppibpc)

# H0 serie estacionaria

testekpsspibpc<-kpss.test(ts_pibpc)

print(testekpsspibpc)

# Calculando a primeira diferença da série

dpibpc = diff(ts_pibpc, lag = 1, differences = 1)

plot(dpibpc, main = "Primeira Diferença PIB pc")

Acf(dpibpc, 10, main = "Primeira Diferença PIB pc")
Pacf(dpibpc, 10, main = "Primeira Diferença PIB pc")

testeadfdpibpc<-adf.test(dpibpc)

print(testeadfdpibpc)

testeppdpibpc<-pp.test(dpibpc)

print(testeppdpibpc)

testekpssdpibpc<-kpss.test(dpibpc)

print(testekpssdpibpc)

# Calculando a segunda diferença

d2PIBpc<-diff(diff(ts_pibpc))

plot(d2PIBpc, main = "Segunda Diferença PIB pc")

Acf(d2PIBpc, 10, main = "Segunda Diferença PIB pc")
Pacf(d2PIBpc, 10, main = "Segunda Diferença PIB pc")

testeadfd2PIBpc<-adf.test(d2PIBpc)

print(testeadfd2PIBpc)

testeppd2PIBpc<-pp.test(d2PIBpc)

print(testeppd2PIBpc)

testekpssd2PIBpc<-kpss.test(d2PIBpc)

print(testekpssd2PIBpc)


# Com relação ao teste ADF apresentar resultados contrários ao teste de Phillips-Perron 
# é necessário observar que no teste ADF se considera que o termo de erro é um Ruído Branco
# ou seja, que tem variância constante e é não autocorrelacionado.
#Se este não for o caso o melhor teste seria o de Phillips-Perron

# Exercicio

dadosts<-read_excel("C:/Aulas_praticas/pib_dpc_rpd.xlsx")

dadosts <- ts(dadosts, start = c(1970,1), end = c(1991,4), frequency = 4)

RPD <- dadosts[ ,4]
PIB <- dadosts[ ,2]
DCP <- dadosts[ ,3]

plot(PIB)
plot(DCP)
plot(RPD)

acf(PIB, 36)
pacf(PIB, 36)

acf(DCP, 36)
pacf(DCP, 36)

acf(RPD, 36)
pacf(RPD, 36)

dPIB<-diff(PIB)
dRPD<-diff(RPD)
dDCP<-diff(DCP)

plot(dPIB)
plot(dRPD)
plot(dDCP)

acf(dPIB, 36)
pacf(dPIB, 36)

acf(dDCP, 36)
pacf(dDCP, 36)

acf(dRPD, 36)
pacf(dRPD, 36)


adf.test(PIB)
adf.test(RPD)
adf.test(DCP)

pp.test(PIB)
pp.test(RPD)
pp.test(DCP)

kpss.test(PIB)
kpss.test(RPD)
kpss.test(DCP)

adf.test(dPIB)
adf.test(dRPD)
adf.test(dDCP)

pp.test(dPIB)
pp.test(dRPD)
pp.test(dDCP)

kpss.test(dPIB)
kpss.test(dRPD)
kpss.test(dDCP)

