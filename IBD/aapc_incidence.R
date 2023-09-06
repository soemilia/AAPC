
#Função para calculo da incidência (Poisson ou Neg binomial)

calcular_aapc_incidence <- function(casos, populacao, anos) {
    library(MASS)
    modelo <- glm.nb(as.numeric(casos) ~ anos + offset(log(populacao)))
    
    #coef. do modelo
    coef_ano <- coef(modelo)[2]
    confianca <- 0.05
    
    zval <- qnorm(1 - confianca / 2)
    SEm <- coef(summary(modelo))[2, "Std. Error"]
    
    #AAPC
    aapc <- (exp(coef_ano) - 1) * 100
    
    # Intervalo de confiança
    ic_upper <- (exp(log((aapc/100) + 1) + (zval * SEm)) - 1) * 100
    ic_lower <- (exp(log((aapc/100) + 1) - (zval * SEm)) - 1) * 100
    
  
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

N_IBD <- calcular_aapc_incidence(Pasta2$`New IBD cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)

N_UC <- calcular_aapc_incidence(Pasta2$`New UC cases`,
                                Pasta2$`Estimated population*`,
                                Pasta2$Year)

N_CD <- calcular_aapc_incidence(Pasta2$`New CD cases`,
                                Pasta2$`Estimated population*`,
                                Pasta2$Year)



cat("Análise de Incidência - New IBD cases:\n",
    "AAPC:", N_IBD$aapc, "%\n",
    "IC 95%:", N_IBD$ic_lower, "-", N_IBD$ic_upper, "%\n",
    "Erro Padrão:", N_IBD$erro_padrao, "\n",
    "Valor de p:", N_IBD$p_valor, "\n")

cat("Análise de Incidência - New UC cases:\n",
    "AAPC:", N_UC$aapc, "%\n",
    "IC 95%:", N_UC$ic_lower, "-", N_UC$ic_upper, "%\n",
    "Erro Padrão:", N_UC$erro_padrao, "\n",
    "Valor de p:", N_UC$p_valor, "\n")

cat("Análise de Incidência - New CD cases:\n",
    "AAPC:", N_CD$aapc, "%\n",
    "IC 95%:", N_CD$ic_lower, "-", N_CD$ic_upper, "%\n",
    "Erro Padrão:", N_CD$erro_padrao, "\n",
    "Valor de p:", N_CD$p_valor, "\n")

