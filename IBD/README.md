# Reprodução de AAPC (Average Annual Percentage Change) de dados de artigo

Dados proveniente do DATASUS de *Inflammatory Bowel Diseases* (IBD) no Brasil.  
Retirados do artigo: [Temporal trends in the epidemiology of inflammatory bowel diseases in the public healthcare system in Brazil: A large population-based study](https://www.sciencedirect.com/science/article/pii/S2667193X22001156?via%3Dihub#bib0018)

#### Estatistica utilizada:
* Análise demostrativa de idade e sexo;
* Average Annual Percentage Change (AAPC) com intervalo de confiança de 95%:
  * Taxa de incidência (regressão de Poisson ou binomial negativa);
  * Taxa de prevalência (regressão log-binomial).
 

#### Arquivos ancorados:
* Pasta2.xlsx: Tabela com os dados;
* aapc_logbinomial.R: Função de cálculo do AAPC para prevalência;
* aapc_incidencia.R: Funçao de cálculo do AAPC para incidência.



#### Leitura para embasar o código:
* [Estimating average annual per cent change in trend analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2843083/)
* [Trend Algorithms](https://seer.cancer.gov/seerstat/WebHelp/Trend_Algorithms.htm)
* [Calculating APC with Weighting](https://seer.cancer.gov/seerstat/WebHelp/Calculating_APC_with_Weighting.htm)
