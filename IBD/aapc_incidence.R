#Função para calculo da incidência (Neg binomial)

calcular_aapc_incidence <- function(casos, populacao, anos) {
        
        modelo <- glm.nb(as.numeric(casos) ~ anos + offset(log(populacao)))
        
        #coef. do modelo
        coef_ano <- coef(modelo)[2]
        confianca <- 0.05
         
        zval <- qnorm(1 - confianca / 2)
        SEm <- coef(summary(modelo))[2, "Std. Error"]
        
        # Intervalo de confiança
        ic_upper <- (exp(coef_ano + (zval * SEm)) - 1) * 100
        ic_lower <- (exp(coef_ano - (zval * SEm)) - 1) * 100
        
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


test2 <- calcular_aapc_binomial(Pasta2$`New IBD cases`,
                               Pasta2$`Estimated population*`,
                               Pasta2$Year)



cat("Análise de Incidência (Poisson/binomial neg.):\n",
    "AAPC2:", test2$aapc, "%\n",
    "IC 95%:", test2$ic_lower, "-", test$ic_upper, "%\n",
    "Erro Padrão:", test2$erro_padrao, "\n",
    "Valor de p:", test2$p_valor, "\n")
