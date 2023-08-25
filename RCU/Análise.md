# Retocolite Ulcerativa (RCU) no Brasil no período de 2012-2022


1. Cálculo da prevalência de RCU na população geral, por análise binomial.
   
```
#Pacote necessário
library(dplyr)
library(readxl)

#Criando o dataframe com os dados
#RCU <- read_excel("C:/Users/soemi/OneDrive/RCU.xlsx", 
#+     col_types = c("numeric", "numeric", "numeric", 
#+         "numeric", "numeric", "numeric", 
#+         "numeric"))

#Ajustando o modelo de regressão binomial
modelo <- glm(cbind(Casos_RCU, Populacao_Estimada - Casos_RCU) ~ Ano, data = dados, family = binomial(link = "logit"))

#Obtendo os coeficientes do modelo
coeficientes <- coef(modelo)

#Calculando o AAPC
aapc <- exp(coeficientes["Ano"]) - 1

#Calculando o erro padrão do AAPC
erro_padrao <- coef(summary(modelo))["Ano", "Std. Error"]

# Calculando o desvio padrão
desvio_padrao_aapc <- erro_padrao * sqrt(length(RCU$Ano))

#Calculando o valor de "p" associado ao AAPC
p_valor <- coef(summary(modelo))["Ano", "Pr(>|z|)"]

#Calculando os intervalos de confiança do AAPC usando a distribuição t
confianca <- 0.95
intervalo_confianca <- c(aapc - qt(1 - confianca / 2, df = length(dados$Ano) - 2) * erro_padrao,
                         aapc + qt(1 - confianca / 2, df = length(dados$Ano) - 2) * erro_padrao)


#Exibindo os resultados

cat("Análise de Prevalência (Binomial):\n",
    "AAPC:", aapc*100, "%\n", 
    "IC 95%:", intervalo_confianca[1] * 100, "-", intervalo_confianca[2] * 100, "%\n",
    "Erro Padrão:", erro_padrao * 100, "\n",
    "Desvio Padrão:", desvio_padrao_aapc *100, "\n",
    "Valor de p:", p_valor, "\n")
```


Resultados

Análise de Prevalência (Binomial):
* AAPC: 15.05531 %
* IC 95%: 15.05322 - 15.0574 %
* Erro Padrão: 0.0323791 
* Desvio Padrão: 0.1073893 
* Valor de p: 0 


 2. 
