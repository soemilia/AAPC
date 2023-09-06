# Função log-binomial - prevalência


calcular_aapc_binomial <- function(casos, populacao, anos) {
        #regressão log-binomial
        modelo <- glm(cbind(casos, populacao - casos) ~ anos,
                     family = binomial(link="log"))
        #coef. do modelo
        coef_ano <- coef(modelo)[2]
        confianca <- 0.05
        zval <- qnorm(1 - confianca / 2)
        SEm <- coef(summary(modelo))[2, "Std. Error"]
        
        # Intervalo de confiança
        ic_upper <- (exp(log((aapc/100) + 1) + (zval * SEm)) - 1) * 100
        ic_lower <- (exp(log((aapc/100) + 1) - (zval * SEm)) - 1) * 100
        
        #AAPC
        aapc <- (exp(coef_ano) - 1) * 100
        
        #Valor de p
         valor_p <- coef(summary(modelo))[2, "Pr(>|z|)"]
        
        resultado <- list(
                p_valor = valor_p,
                ic_lower = ic_lower,
                ic_upper = ic_upper,
                erro_padrao = SEm,
                aapc = aapc,
                modelo = modelo
        )
        
        return(resultado)
}

test <- calcular_aapc_binomial(Pasta2$`IBDU cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)



cat("Análise de Prevalência (Binomial):\n",
    "AAPC:", test$aapc, "%\n",
    "IC 95%:", test$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", test$erro_padrao, "\n",
    "Valor de p:", test$p_valor, "\n")


## valor esperado de acordo com artigo: 
## AAPC: 19.1%
## IC 95%: 18.84 - 19.41%
## Erro Padrão:
## Valor de p < 0.0001


