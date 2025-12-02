# Modelos com dados em painel

library(tidyverse)
library (readxl)
library(stargazer)
library(plm)
library(writexl)
library(lmtest)
library(knitr)
library(kableExtra)
library (car)
library(reshape2)
library (MASS)
library(sandwich)
library(patchwork)

# Importa os dados para modelagem
sojaBR <- read_xlsx("C:/Aulas_praticas/dados_soja_Painel.xlsx", sheet = "FOR_R_alt", range="B2:AB266")

# Transforma base de dados para dados em painel
sojaBR_painel <- pdata.frame(sojaBR, index = c("Estado", "Season"), drop.index = FALSE)

# Variáveis selecionadas para modelar
soja_lean <- Yield ~ 
  LAI_acc +
  NDVI_max +
  def_exc_acc +
  tavg + 
  precip +
  Safra +
  Tendencia

# Modelo Pooled - OLS
OLS <- plm(formula = soja_lean, data = sojaBR_painel, model = "pooling")
summary (OLS)
stargazer(OLS, 
          type = "text", 
          title = "Resultados do Modelo MQO", 
          dep.var.labels=c("Produtividade Final da Soja"), 
          single.row = T, 
          style = "all", 
          digit.separator = ",", 
          digits = 3)


# Modelo Efeitos Fixos - MEF intraunidade
MEF <- plm(formula = soja_lean, data = sojaBR_painel, model = "within")
summary(MEF)
stargazer(MEF, 
          type = "text", 
          title = "Resultados do Modelo MEF", 
          dep.var.labels=c("Produtividade Final da Soja"), 
          single.row = T, 
          style = "all", 
          digit.separator = ",", 
          digits = 3)


# Modelo Efeitos Fixos - MEF com variáveis indicadoras por unidade
MEF_dum1 <- lm(Yield ~ LAI_acc 
               + NDVI_max 
               + def_exc_acc 
               + tavg 
               + precip 
               + Safra 
               + Tendencia 
               + factor(Estado), 
               data = sojaBR)
summary(MEF_dum1)
stargazer(MEF_dum1, 
          type = "text", 
          title = "Resultados do Modelo MEF", 
          dep.var.labels=c("Produtividade Final da Soja"), 
          single.row = T, 
          style = "all", 
          #digit.separator = ",", 
          digits = 3)


# Modelo Efeitos Aleatorios - MEA
MEA <- plm(formula = soja_lean, data = sojaBR_painel, model = "random", random.method = "walhus")
summary (MEA)
stargazer(MEA, 
          type = "text", 
          title = "Resultados do Modelo MEA", 
          dep.var.labels=c("Produtividade Final da Soja"), 
          single.row = T, 
          style = "all", 
          digit.separator = ",", 
          digits = 3)


# OLS X MEF
# se p-value <0.05, rejeita-se H0, portanto preferencia por MEF
# Conclusão
# Como você rejeita a hipótese nula, isso sugere que os efeitos individuais (os estados) são significativos no modelo. Em outras palavras, a inclusão de efeitos fixos melhora significativamente a explicação do modelo em comparação com o modelo OLS, que assume que os coeficientes são os mesmos para todos os indivíduos.
pFtest(MEF,OLS)


# OLS x MEA
# se p-value <0.05, rejeita-se H0, portanto preferencia por MEA
# Como você rejeita a hipótese nula, isso indica que há efeitos significativos em seu modelo que justificam o uso de um modelo de efeitos aleatórios.
# Implicações
# Esse resultado sugere que a variabilidade entre os estados é relevante e que um modelo de efeitos aleatórios pode ser mais apropriado, dependendo dos seus testes anteriores (como o teste de pooling e o teste pF). Entretanto, a escolha entre modelos de efeitos fixos e aleatórios deve considerar a interpretação dos efeitos e a estrutura dos dados.
plmtest (OLS, type='bp')

# MEF x MAE
# se p-value <0.05, rejeita-se H0, portanto preferencia por MEF
# Conclusão
# Como você rejeita a hipótese nula, isso sugere que um dos modelos (ou ambos) pode ser inconsistente. Geralmente, quando o teste de Hausman indica rejeição, isso sugere que o modelo de efeitos fixos (MEF) é preferível ao modelo de efeitos aleatórios (MEA). Isso ocorre porque os efeitos fixos assumem que as características não observadas que variam entre os estados estão correlacionadas com as variáveis independentes, o que pode levar a estimativas viesadas se um modelo de efeitos aleatórios for usado.
# Implicações
# Dado que o modelo de efeitos fixos é mais apropriado, você deve considerar usá-lo para suas análises futuras, uma vez que ele lida melhor com a heterogeneidade não observada entre os estados.
phtest (MEF, MEA)


# Teste Durbin-Watson para autocorrelação dos erros.
# H0: Não há autocorrelação no termo do erro. P value < 5% rejeita a hipotese nula
pdwtest (soja_lean, data = sojaBR_painel, model = "within")


# Comparaçao dos 3 métodos
stargazer(OLS,MEF, MEF_dum1, MEA, 
          type = "text", 
          title = "Comparativo dos 4 Métodos de Modelagem em Painel", 
          dep.var.labels=c("Produtividade Final da Soja"), 
          single.row = T, 
          style = "all", 
          digit.separator = ",", 
          digits = 3,
          column.labels = c("Empilhado - MQO",  "MEF - Efeitos Fixos intraunidade", "MEF - Efeitos Fixos com variávies indicadoras", "MEA - Efeito Aleatório")
)

# Previsao de cada abordagem
# Passo 1: Obter valores preditos
sojaBR$predictedOLS <- predict(OLS)
sojaBR$predictedMEF <- predict(MEF)
sojaBR$predictedMEF_dum1 <- predict(MEF_dum1)
sojaBR$predictedMEA <- predict(MEA)

# Passo 2: Criar um data frame com os valores reais e preditos
resultadosOLS <- data.frame(Real = sojaBR$Yield,
                            Predito = sojaBR$predictedOLS)

resultadosMEF <- data.frame(Real = sojaBR$Yield,
                            Predito = sojaBR$predictedMEF)

resultadosMEF_dum1 <- data.frame(Real = sojaBR$Yield,
                                 Predito = sojaBR$predictedMEF_dum1)

resultadosMEA <- data.frame(Real = sojaBR$Yield,
                            Predito = sojaBR$predictedMEA)

# Passo 3: Criar o gráfico
plot_OLS <- ggplot(resultadosOLS, aes(x = Real, y = Predito)) +
  geom_point(alpha = 1) +  # Adiciona os pontos
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +  # Linha de referência
  labs(
    title = "MQO",
    x = "Real - kg/ha",
    y = "Predito - kg/ha"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Aumenta o espaço acima do título do eixo X
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),  # Aumenta o espaço à direita do título do eixo Y
    axis.text = element_text(size = 14)
  )

plot_MEF <- ggplot(resultadosMEF, aes(x = Real, y = Predito)) +
  geom_point(alpha = 1) +  # Adiciona os pontos
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +  # Linha de referência
  labs(
    title = "MEF - efeitos fixos intraunidade",
    x = "Real - kg/ha",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.text = element_text(size = 14)
  )

plot_MEF_dum1 <-ggplot(resultadosMEF_dum1, aes(x = Real, y = Predito)) +
  geom_point(alpha = 1) +  # Adiciona os pontos
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +  # Linha de referência
  labs(
    title = "MEF - variáveis indicadoras por unidade",
    x = "Real - kg/ha",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.text = element_text(size = 14)
  )

plot_MEA <-ggplot(resultadosMEA, aes(x = Real, y = Predito)) +
  geom_point(alpha = 1) +  # Adiciona os pontos
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +  # Linha de referência
  labs(
    title = "MEA",
    x = "Real - kg/ha",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.text = element_text(size = 14)
  )

combined_plot <- plot_OLS + plot_MEF + plot_MEF_dum1 + plot_MEA + plot_layout(ncol = 4)
print(combined_plot)




