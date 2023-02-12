
# Carregando os dados -----------------------------------------------------


require(dplyr)
require(readr)
require(ggplot2)

dados <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")


# -------------------------------------------------------------------------


# Analise exploratoria ----------------------------------------------------

# variavel de interesse é Atrition (saída do funcionário)

table(dados$Attrition)



# -------------------------------------------------------------------------


