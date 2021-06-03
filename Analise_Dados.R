setwd("E:/Estudos/Trabalho_Bloco_A")
getwd()

library(dplyr)
library(tidyr) #adc por felipe para transpor a coluna em varias colunas
library(ggplot2)
#install.packages("viridis")
library(viridis)
#install.packages("patchwork")
library(patchwork) # Mostrar dois gráficos ao lado


df_total <- read.csv("df_final.csv")
View(df_total)


#Primeiramente vou gerar um novo dataset filtrando apenas o Brasil para analise (sem levar em consideração por região)
df_total_brasil <- subset(df_total, Sigla_regiao== 'BR')

#Após feito a junção dos dados em um único dataset, será plotado algumas visões

#Gráfico da variação do dolar e do barril de petroleo (brent)
linhaDolar <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = CotacaoDolar, group=1), color = "darkred", size=2)+
   ggtitle("Cotação dólar 2018-07 até 2020-07") +
   ylab("Cotação")

linhaBrent <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = CotacaoBrentReais, group=2), color = "steelblue", size=2)+
   ggtitle("Cotação Barril em Real 2018-07 até 2020-07") +
   ylab("Cotação")  

linhaDolar+linhaBrent


#gráfico da variação do preço da gasolina pelo produtor
linhaVarProdutor <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = mediaPrecoGasolina, group=1), color = "darkred", size=2)+
   ggtitle("Preço produtor gasolina 2018-07 até 2020-07") +
   ylab("Cotação")

linhaDolar+linhaBrent+linhaVarProdutor

