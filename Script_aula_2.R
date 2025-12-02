# Pacotes necessarios para a aula 02/04

library(readxl)
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(knitr)
library(TSstudio)
library(xts)

# Aula 02/04 - Modelos ARIMA

#  Criando a serie temporal mensal

FornitureSales<-read_csv("C:/Aulas_praticas/Forniture_Sales.csv")

Vendas<-ts(FornitureSales [2], start = c(1992,1), end = c(2019,9), frequency = 12)

plot(Vendas, main = "Vendas de Moveis")

# Decomposição da serie temporal

decomp_vendas <- decompose(Vendas, type = "additive")

plot(decomp_vendas)

# Teste de raiz unitaria

adf.test(Vendas)
kpss.test(Vendas)

dVendas<-diff(Vendas, lag = 1, differences = 1)

plot(dVendas)

decomp_dvendas <- decompose(dVendas, type = "additive")

plot(decomp_dvendas)

adf.test(dVendas)
kpss.test(dVendas)

# Modelos ARIMA

arima110<-Arima(Vendas, order=c(1,1,0))

summary(arima110)

arima011<-Arima(Vendas, order=c(0,1,1))

summary(arima011)

arima111<-Arima(Vendas, order=c(1,1,1))

summary(arima111)

arima211<-Arima(Vendas, order=c(2,1,1))

summary(arima211)

arima212<-Arima(Vendas, order=c(2,1,2))

summary(arima212)

# Entre esses o melhor modelo é o Arima(2,1,2)

# Vimos na decomposição que existe um componente de sazonilidade importante
# Vamos estimar um modelo SARIMA

sarima<-Arima(Vendas, order=c(2,1,2), seasonal = c(1,1,1))

summary(sarima)

# A especificação do SARIMA foi melhor que o ARIMA

# Comando auto.arima

auto_arima<-auto.arima(Vendas)

summary(auto_arima)

# a função auto arima nos da o melhor modelo de acordo com os criterios de informação.
# Nesse exemplo o melhor modelo é um SARIMA(2,1,0)(0,1,2)[12]

# Vamos comparar as previsões do Arima(2,1,2) com o SARIMA(2,1,0)(0,1,2)[12]

forecast(arima212, h = 6)

forecast(sarima, h=6)

forecast(auto_arima, h = 6)

autoplot(forecast(arima212, h = 6), PI = FALSE)

autoplot(forecast(sarima, h = 6), PI = FALSE)

autoplot(forecast(auto_arima, h = 6), PI = FALSE)

autoplot(forecast(sarima, h = 12), PI = FALSE)

autoplot(forecast(auto_arima, h = 12), PI = FALSE)

autoplot(forecast(sarima, h = 18), PI = FALSE)

autoplot(forecast(auto_arima, h = 18), PI = FALSE)

autoplot(forecast(sarima, h = 12))

autoplot(forecast(auto_arima, h = 12))

autoplot(forecast(sarima, h = 18))

autoplot(forecast(auto_arima, h = 18))


# Exercio refazer todos os passo com ln das Vendas

ln_vendas<-log(Vendas)

plot(ln_vendas)

decomp_lnvendas <- decompose(ln_vendas, type = "additive")

plot(decomp_lnvendas)

# Teste de raiz unitaria

adf.test(ln_vendas)
kpss.test(ln_vendas)

dlnvendas<-diff(ln_vendas, lag = 1, differences = 1)

plot(dlnvendas)

decomp_dlnvendas <- decompose(dlnvendas, type = "additive")

plot(decomp_dlnvendas)

adf.test(dlnvendas)
kpss.test(dlnvendas)

# Modelos ARIMA

arima110ln<-Arima(ln_vendas, order=c(1,1,0))

summary(arima110ln)

arima011ln<-Arima(ln_vendas, order=c(0,1,1))

summary(arima011ln)

arima111ln<-Arima(ln_vendas, order=c(1,1,1))

summary(arima111ln)

arima211ln<-Arima(ln_vendas, order=c(2,1,1))

summary(arima211ln)

arima212ln<-Arima(ln_vendas, order=c(2,1,2))

summary(arima212ln)

# Entre esses o melhor modelo é o Arima(2,1,2)

# Comando auto.arima

auto_arimaln<-auto.arima(ln_vendas)

summary(auto_arimaln)

# a função auto arima nos da o melhor modelo de acordo com os criterios de informação.
# Nesse exemplo o melhor modelo é um SARIMA(2,1,0)(0,1,2)[12]

# Vamos comparar as previsões do Arima(2,1,2) com o SARIMA(2,1,0)(0,1,2)[12]

forecast(arima212, h = 6)

forecast(auto_arima, h = 6)

autoplot(forecast(arima212ln, h = 6), PI = FALSE)

autoplot(forecast(auto_arimaln, h = 6), PI = FALSE)

autoplot(forecast(auto_arimaln, h = 12), PI = FALSE)

autoplot(forecast(auto_arimaln, h = 18), PI = FALSE)

autoplot(forecast(auto_arimaln, h = 12))

autoplot(forecast(auto_arimaln, h = 18))

