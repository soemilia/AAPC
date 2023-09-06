# Função log-binomial


calcular_aapc_binomial <- function(casos, populacao, anos) {
        #regressão log-binomial
        modelo <- glm(cbind(casos, populacao - casos) ~ anos,
                     family = binomial(link="log"))
        #coef. do modelo
        confianca <- 0.05
        
        zval <- qnorm(1 - confianca / 2)
        SEm <- coef(summary(modelo))[2, "Std. Error"]

        #AAPC
        aapc <- (exp(coef_ano) - 1) * 100
        
        # Intervalo de confiança
        ic_upper <- (exp(log((aapc/100) + 1 + (zval * SEm)) - 1) * 100
        ic_lower <- (exp(log((aapc/100) + 1 - (zval * SEm)) - 1) * 100
        
       
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

IBDU <- calcular_aapc_binomial(Pasta2$`IBDU cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)
CD <- calcular_aapc_binomial(Pasta2$`CD cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)
UC <- calcular_aapc_binomial(Pasta2$`CD cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)
IBD <- calcular_aapc_binomial(Pasta2$`CD cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)



cat("Análise de Prevalência - IBD:\n",
    "AAPC:", IBD$aapc, "%\n",
    "IC 95%:", IBD$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", IBD$erro_padrao, "\n",
    "Valor de p:", IBD$p_valor, "\n")

cat("Análise de Prevalência - UC:\n",
    "AAPC:", UC$aapc, "%\n",
    "IC 95%:", UC$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", UC$erro_padrao, "\n",
    "Valor de p:", UC$p_valor, "\n")

 cat("Análise de Prevalência - CD:\n",
    "AAPC:", CD$aapc, "%\n",
    "IC 95%:", CD$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", CD$erro_padrao, "\n",
    "Valor de p:", CD$p_valor, "\n")

cat("Análise de Prevalência- IBDU:\n",
    "AAPC:", IBDU$aapc, "%\n",
    "IC 95%:", IBDU$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", IBDU$erro_padrao, "\n",
    "Valor de p:", IBDU$p_valor, "\n")


