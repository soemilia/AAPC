# Retocolite Ulcerativa (RCU) no Brasil no período de 2012-2022

### Pré-análise: Distibuição Poisson x Binomial negativa
Após análise das [saídas](https://github.com/soemilia/epi_aapc/blob/main/RCU/Binomial_Poisson_Sa%C3%ADda) do código [Binomial_Poisson](https://github.com/soemilia/epi_aapc/blob/main/RCU/Binomial_Poisson), foi observado que para as 5 análises ( Proporção_RCU_BRASIL, cx/RCU_Acumulativo, cx/RCU_nãoAcumulativo, Hosp/RCU_Acumulativo e Hosp/RCU_nãoAcumulativo) apresentam distribuição binomial negativa.

## Breakspoints 
Pode-se usar funções como a `selgmented()`,  `pscore.test()` ou mesmo `davies.test()` para avaliar a presença de breakpoints na regressão.

```
library(segmented)
library(nlme)

selgmented(RCU_BR)
```

Saída:
```
Hypothesis testing to detect no. of breakpoints
statistic: Score   level: 0.05   Bonferroni correction: FALSE 
p-value '0 vs 2' = 0.01128   p-value '1 vs 2' = 0.6182 
Overall p-value = 0.0416
No. of selected breakpoints:  1
```

Estimativa do breakpoint pode ser avaliado pela função `segmented()`

``` 
fit.seg<-segmented(RCU_BR, seg.Z=~Ano)

```

Saída

```
>fit.seg
Call: segmented.glm(obj = RCU_BR, seg.Z = ~Ano)

Meaningful coefficients of the linear terms:
(Intercept)          Ano       U1.Ano  
  -466.0560       0.2273      -0.1080  

Estimated Break-Point(s):
psi1.Ano  
    2015  

Degrees of Freedom: 10 Total (i.e. Null);  7 Residual
Null Deviance:     0.0803 
Residual Deviance: 0.0793      AIC: 8.084 
```
### Função para a análise de AAPC por regressão binomial negativa

```
#Pacote necessário
library(dplyr)
library(readxl)
library(segmented)
library(nlme)
library(MASS)

#Criando o dataframe com os dados
#RCU <- read_excel("C:/Users/soemi/OneDrive/RCU.xlsx", 
#+     col_types = c("numeric", "numeric", "numeric", 
#+         "numeric", "numeric", "numeric", 
#+         "numeric"))


# função para o calculo da prevalencia
calcular_aapc_binomial <- function(eventos, anos) {
        modelo <- glm.nb(eventos ~ anos)
        coef_ano <- coef(modelo)[2]
      
        erro_padrao <- coef(summary(modelo))[2, "Std. Error"]
        graus_liberdade_residual <- df.residual(modelo)
        confianca <- 0.95
        graus_liberdade <- graus_liberdade_residual - 2  
        tval <- qt(1 - confianca / 2, df = graus_liberdade)
        SEm <- coef(summary(modelo))[2, "Std. Error"]
        
        ic_upper <- (exp(coef_ano + (Tval * SEm)) - 1) * 100
        ic_lower <- (exp(coef_ano - (Tval * SEm)) - 1) * 100
        aapc2 <- (exp(coef_ano) - 1) * 100
        p_valor <- coef(summary(modelo))[2, "Pr(>|z|)"]
        
        valor_absoluto_m_SEm <- abs(coef_ano / SEm)
        graus_liberdade_residual <- df.residual(modelo)
        valor_p <- 2 * pt(-valor_absoluto_m_SEm, df = graus_liberdade_residual)
        
        resultado <- list(
                p_valor = valor_p,
                ic_lower = ic_lower,
                ic_upper = ic_upper,
                erro_padrao = erro_padrao,
                aapc2 = aapc2,
                modelo = modelo
        )
        
        return(resultado)
}


```

### 1. Cálculo da prevalência de RCU na população geral, por análise binomial negativa.
   


```
# Input
RCU_BR_prev <- calcular_aapc_binomial(RCU$Casos_RCU, RCU$Ano)
cat("Análise de Prevalência (Binomial):\n",
    "AAPC2:", RCU_BR_prev$aapc2, "%\n",
    "IC 95%:", RCU_BR_prev$ic_lower, "-", RCU_BR_prev$ic_upper, "%\n",
     "Erro Padrão:", RCU_BR_prev$erro_padrao, "\n",
     "Valor de p:", RCU_BR_prev$p_valor, "\n"
```

```
# Output
Análise de Prevalência (Binomial):
 AAPC2: 16.86037 %
 IC 95%: 16.7986 - 16.92217 %
 Erro Padrão: 0.008021005 
 Valor de p: 1.174076e-08 
```
 
### 2. Cálculo da taxa de CX em casos de RCU, por análise binomial negativa.


