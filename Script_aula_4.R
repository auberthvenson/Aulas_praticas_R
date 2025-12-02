# Pacotes necessarios para a aula 16/04

library(haven)
library(MASS)
library(lmtest)
library(sandwich)
library(mfx)

# Aula 16/04 - Modelos logit e probit

# Abrindo a base de dados

saneamento<-read_dta("C:/Aulas_praticas/base_demanda_san_rural.dta")

# Estimando o modelo logit

logit<-glm(col_lixo ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO , data = saneamento, family = binomial(link = "logit"))

summary(logit)

# o modelo logit tem naturalmente hetocedasticidade, para resolver usamos
# erros-padrão robustos

robust_se<-vcovHC(logit, type = ("HC1"))
print(robust_se)

teste_robusto <- coeftest(logit, vcov = robust_se)
print(teste_robusto)

# odds-ratios

parametros<-coef(logit)

print(parametros)

odds<-exp(parametros)

print(odds)

logitor<-logitor(col_lixo ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO , data = saneamento, robust = TRUE)

print(logitor)

# efeitos marginais

logitmfx<-logitmfx(col_lixo ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO , data = saneamento, robust = TRUE)

print(logitmfx)

# criterios de informacao

BIC(logit)
AIC(logit)

# pseudo R2

nullmodel <- glm(col_lixo ~ 1, data = saneamento, family = binomial(link = "logit"))

1 - (logLik(logit) / logLik(nullmodel))


# matriz de confusao, acuracia, sensistividade e especificidade

# Retorna as probabilidades previstas

predicted <- predict(logit, newdata = saneamento, type = "response")

# Definir um limiar de probabilidade para a classificacao
threshold <- 0.5  # Pode ajustar conforme necessário

# Classificar as previsoes com base no limiar de probabilidade
predicted_class <- ifelse(predicted > threshold, 1, 0)

# Criar a matriz de confusão
conf_matrix <- table(observed = saneamento$col_lixo, predicted = predicted_class)

print(conf_matrix)

# Calcular a acurácia
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Calcular a sensibilidade
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calcular a especificidade
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

# Exibir as métricas
print(paste("Acurácia:", accuracy))
print(paste("Sensibilidade:", sensitivity))
print(paste("Especificidade:", specificity))

# modelo probit 

probit<-glm(col_lixo ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO , data = saneamento, family = binomial(link = "probit"))

summary(probit)

# efeitos marginais

probitmfx<-probitmfx(col_lixo ~ ln_renda + npesfam + idade_rf + mulher_rf + anosesc_rf + comodos + eletric + alvenaria + agua_enc + NE + SU + NO + CO , data = saneamento, robust = TRUE)

print(probitmfx)

# criterios de informacao

BIC(probit)
AIC(probit)

# pseudo R2

nullmodel2 <- glm(col_lixo ~ 1, data = saneamento, family = binomial(link = "probit"))

1 - (logLik(probit) / logLik(nullmodel2))

# matriz de confusao, acuracia, sensistividade e especificidade

# Retorna as probabilidades previstas

predicted2 <- predict(probit, newdata = saneamento, type = "response")

# Classificar as previsoes com base no limiar de probabilidade
predicted_class2 <- ifelse(predicted2 > threshold, 1, 0)

# Criar a matriz de confusão
conf_matrix2 <- table(observed = saneamento$col_lixo, predicted2 = predicted_class2)

print(conf_matrix2)

# Calcular a acurácia
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)

# Calcular a sensibilidade
sensitivity2 <- conf_matrix2[2, 2] / sum(conf_matrix2[2, ])

# Calcular a especificidade
specificity2 <- conf_matrix2[1, 1] / sum(conf_matrix2[1, ])

# Exibir as métricas
print(paste("Acurácia:", accuracy2))
print(paste("Sensibilidade:", sensitivity2))
print(paste("Especificidade:", specificity2))