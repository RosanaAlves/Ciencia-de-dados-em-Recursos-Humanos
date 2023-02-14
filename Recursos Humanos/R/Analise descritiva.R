
# Carregando os dados -----------------------------------------------------


require(dplyr)
require(readr)
require(ggplot2)
library(tidyverse)
library(caret)

dados <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# -------------------------------------------------------------------------


# Analise exploratoria ----------------------------------------------------

## variavel de interesse é Atrition (saída do funcionário)

summary(dados)

Dep <- table(dados$Department)
# pairs(dados)

attritionY <- dados%>%
  filter(Attrition == "Yes")
DepY <- table(attritionY$Department)

## Proporção de sáidas de funcionários por departamento

SpD <- c()
for(i in 1:3){
  SpD[i] = (DepY[i]/Dep[i])*100
}

## O departamento que tem mais pedidos de demissão é o de vendas (Sales), seguido pelo recursos humanos e por fim pesquisa e desenvolvimento.

# -------------------------------------------------------------------------


# Visualizações -----------------------------------------------------------
# library(DataEditR)
# library(fastDummies) ## data_edit(dados)

AttritionDummy <- c()
for(i in 1:length(dados$Attrition)){
  if(dados$Attrition[i] == "Yes"){
    AttritionDummy[i] = 1
  } else {AttritionDummy[i] = 0}
}
table(AttritionDummy)
table(dados$Attrition)

dados$Attrition <- AttritionDummy

#Proporçoes

prop.table(table(dados$Attrition,dados$Gender)) #Tabela cruzada, Da a proporçao de cada variavel em cada categoria em relaçao ao total
AttGen <- table(dados$Attrition,dados$Gender)
propG <- c()#Proporção de pedido de demissão por genero
for(i in 1:2){
  propG[i] =  AttGen[2,i]/( AttGen[1,i]+  AttGen[2,i])
}

### A propoçao de demissões por gênero é maior para os homens, isso mostra que homens pediram mais demissões do que as mulheres.

ggplot(dados) +
  geom_bar( aes(x=Gender, y=Attrition), stat="identity", fill="skyblue", alpha=0.7)+
  labs(title = "Pedidos de demissão por Gênero")



ggplot(dados) +
  geom_bar( aes(x=EducationField, y=Attrition), stat="identity", fill="skyblue", alpha=0.7)



ggplot(dados) +
  geom_bar( aes(x=Age, y=Attrition), stat="identity", fill="skyblue", alpha=0.7)
