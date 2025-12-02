# Pacotes necessarios para a aula 09/04

library(readxl)
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(knitr)
library(TSstudio)
library(xts)
library(lmtest)
library(urca)
install.packages("vars")
library(vars)

# Aula 09/04 - Modelo VAR e teste de causalidade de Granger

# Abrindo a base de dados

dados_chuva <- read_excel("C:/Aulas_praticas/dados_chuva.xlsx")

# Criando as series de tempo

ts_chuva <- ts(dados_chuva, start = c(2003,1), end = c(2022,12), frequency = 12) 


# Teste de estacionaridade
adf.test(dados_chuva$precipitacao)
adf.test(dados_chuva$queimadas)

# Grafico das series de tempo
plot(ts_chuva)

# Modelo de VAR(1)

var1 <- VAR(ts_chuva, p = 1)

summary(var1)

# VAR(1) com constante

var1c<-VAR(ts_chuva, p=1, type = c("const"))

summary(var1c)

# VAR(1) com constante e tendencia

var1ct<-VAR(ts_chuva, p=1, type=c("both"))

summary(var1ct)

# VAR(2) com constante e tendencia

var2<-VAR(ts_chuva, p=2, type=c("both"))

summary(var2)

# funcao impulso-resposta

# Realize a analise com impulso em precipitacao
irf_prec<- irf(var2, impulse = "precipitacao", response = c("queimadas", "precipitacao"), n.ahead = 12)

# Plote os resultados da analise de impulso-resposta
plot(irf_prec)

# Realize a analise com impulso em queimadas
irf_quei<- irf(var2, impulse = "queimadas", response = c("precipitacao", "queimadas"), n.ahead = 2)

# Plote os resultados da analise de impulso-resposta
plot(irf_quei)


# Teste de Causalidade de Granger

#aplicar o teste queimadas granger-causa precipitação
lmtest:: grangertest(
  y  = ts_chuva[,"precipitacao"],
  x  = ts_chuva[, "queimadas"],
  order = 1
)

lmtest:: grangertest(
  y  = ts_chuva[,"precipitacao"],
  x  = ts_chuva[, "queimadas"],
  order = 2
)

#aplicar o teste precipitacao granger-causa queimadas

lmtest:: grangertest(
  y  = ts_chuva[,"queimadas"],
  x  = ts_chuva[, "precipitacao"],
  order = 1
)

lmtest:: grangertest(
  y  = ts_chuva[,"queimadas"],
  x  = ts_chuva[, "precipitacao"],
  order = 2
)