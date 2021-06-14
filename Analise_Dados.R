setwd("E:/Estudos/Trabalho_Bloco_A")
getwd()

library(dplyr)
library(tidyr) #adc por felipe para transpor a coluna em varias colunas
library(ggplot2)
#install.packages("viridis")
library(viridis)
#install.packages("patchwork")
library(patchwork) # Mostrar dois gráficos ao lado
library(modelr)
#library(tidyverse)
library(corrplot)
#install.packages("corrplot")
library(psych)
#install.packages("psych")


df_brasil <- read.csv("df_brasil.csv")


#verificando linhas duplicadas
sum(duplicated(df_brasil))

#verifica se tem registros nulos
sum(is.na(df_brasil))

#Estatísticas básicas: mínimos, máximos, médias, desvios, etc. de cada variável;
summary(df_brasil, maxsum = max(lengths(lapply(df_brasil, unique))))

View(df_brasil)

# plot


colors()



#Transpor o dataframe de colunas para linhas
# df <- df_mediana%>%
#   select(mes, Sigla_regiao, CotacaoDolar, CotacaoBrentReais,medianaDistrTransporte,medianaRevenda,medianaPrecoEtanol,medianaPrecoGasolina,medianaTribEstadual,medianaTribEstadual,medianaTribFederal,MedianaIPCAMes,medianaGasolinaBomba) %>%
#   gather(key = "variable", value = "value", -mes,-Sigla_regiao)
# 
# View(df)
# #Mescla colunas regiao e impostos/cotacao/brent(variable)
# df$Regiao_variable <- paste(df$Sigla_regiao,df$variable)
# 
# #eliminar linhas repetidas de cotacao/brent(reais e dolar)
# df<- df%>%filter(
#   Regiao_variable != 'SE CotacaoDolar' & Regiao_variable != 'SE CotacaoBrentReais' & Regiao_variable != 'SE ValorEmDolar'&
#     Regiao_variable != 'S CotacaoDolar' & Regiao_variable != 'S CotacaoBrentReais' & Regiao_variable != 'S ValorEmDolar'&
#     Regiao_variable != 'NE CotacaoDolar' & Regiao_variable != 'NE CotacaoBrentReais' & Regiao_variable != 'NE ValorEmDolar'&
#     Regiao_variable != 'CO CotacaoDolar' & Regiao_variable != 'CO CotacaoBrentReais' & Regiao_variable != 'CO ValorEmDolar'&
#     Regiao_variable != 'N CotacaoDolar' & Regiao_variable != 'N CotacaoBrentReais' & Regiao_variable != 'N ValorEmDolar')
# 




#Primeiramente vou gerar um novo dataset filtrando apenas o Brasil para analise (sem levar em consideração por região)
# df_brasil <- subset(df_total, Sigla_regiao== 'BR')
# 
# View(df_brasil)
# 
# str(df_brasil)
# 
# 
# 
# 
# 
# str(df_media)
# 
# linhaGasolinaBomba <- ggplot(data=df_media, aes(x=mes, color = Sigla_regiao,y = mediaGasolinaBomba, group=Sigla_regiao)) +
#   geom_line(size=1) +
#   ggtitle("Cotação dólar 2018-07 até 2020-07") +
#   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# linhaGasolinaBomba
# 
# View(df_media)


# linhaGasolinaBomba <- ggplot(data=df_media, aes(x=mes, color = Sigla_regiao,y = mediaGasolinaBomba, group=Sigla_regiao)) +
#   geom_line(size=1) +
#   ggtitle("Cotação dólar 2018-07 até 2020-07") +
#   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   geom_line(aes(y = CotacaoDolar), color = "darkred",group = 1,size=2)
# 
# linhaGasolinaBomba


#Matriz de correlação


df_brasil_cor <- df_brasil

#converter para inteiro
for(i in 1:ncol(df_brasil_cor)){
  
  df_brasil_cor[,i]<- as.numeric(df_brasil_cor[,i])
}

str(df_brasil_cor)

View(df_brasil_cor)


corrplot(cor(df_brasil_cor,method = "pearson"), method = "number", type = "lower")

corrplot(cor(df_brasil_cor,method = "spearman"), method = "number", type = "lower")

corrplot(cor(df_brasil_cor,method = "pearson"), method = "circle", type = "lower")

corrplot(cor(df_brasil_cor,method = "spearman"), method = "circle", type = "lower")

corrplot(cor(df_brasil_cor), method = "circle", type = "lower")

#Explica númericamente a correlação
cor(df_brasil_cor,method = "pearson")

cor(df_brasil_cor,method = "spearman")


#Podemos observar que há correlação do preço do produtor com o preço do barril de petróleo e a cotação do dolar
#Abaixo será plotado a curva do brent, cotacao dolar e o preço do produtor

#Gráfico da variação do dolar e do barril de petroleo (brent)
linhaDolar <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = Dolar, group=1), color = "darkred", size=2)+
  ggtitle("Cotação dólar") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaDolar

linhaBrent <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = CtBarril, group=2), color = "steelblue", size=2)+
  ggtitle("Cotação Barril em Real") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))  


#gráfico da variação do preço da gasolina pelo produtor
linhaVarGas <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = GasolNaBomba, group=1), color = "darkred", size=2)+
  ggtitle("Preço gasolina") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaIBC <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = IBC, group=1), color = "darkred", size=2)+
  ggtitle("IBC") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaDolar+linhaBrent+linhaVarGas+linhaIBC





#pela cotação do dolar e do barril de petróleo vemos a curva causada pelo corona começou em 01/2020, então para motivo de análise vamos dividir em dois períodos
# a correlação, antes do covid e pós covid

df_brasil_pre_cor<-df_brasil%>%filter(as.character(mes) < "2020-01")

df_brasil_pos_cor<-df_brasil%>%filter(as.character(mes) >= "2020-01")

View(df_brasil_pre_cor)

View(df_brasil_pos_cor)

#pre covid
str(df_brasil_pre_cor)


df_brasil_pre_cor_cor <- df_brasil_pre_cor

#converter para inteiro
for(i in 1:ncol(df_brasil_pre_cor)){
  
  df_brasil_pre_cor_cor[,i]<- as.numeric(df_brasil_pre_cor[,i])
}

View(df_brasil_pre_cor_cor)

str(df_brasil_pre_cor_cor)


corrplot(cor(df_brasil_pre_cor_cor[-1],method = "pearson"), method = "number", type = "lower")

corrplot(cor(df_brasil_pre_cor_cor[-1],method = "pearson"), method = "circle", type = "lower")

corrplot(cor(df_brasil_pre_cor_cor[-1],method = "spearman"), method = "number", type = "lower")

corrplot(cor(df_brasil_pre_cor_cor[-1],method = "pearson"), method = "circle", type = "lower")


#pos covid

str(df_brasil_pos_cor)


df_brasil_pos_cor_cor <- df_brasil_pos_cor

#converter para inteiro
for(i in 1:ncol(df_brasil_pos_cor)){
  
  df_brasil_pos_cor_cor[,i]<- as.numeric(df_brasil_pos_cor[,i])
}

View(df_brasil_pos_cor_cor)

str(df_brasil_pos_cor_cor)


corrplot(cor(df_brasil_pos_cor_cor[-1],method = "pearson"), method = "number", type = "lower")

corrplot(cor(df_brasil_pos_cor_cor[-1],method = "pearson"), method = "circle", type = "lower")

corrplot(cor(df_brasil_pos_cor_cor[-1],method = "spearman"), method = "number", type = "lower")

corrplot(cor(df_brasil_pos_cor_cor[-1],method = "pearson"), method = "circle", type = "lower")





#Observando as curvas vemos uma correlação positiva entre barril de petroleo e o repasse do preço do produtor
#Além disso na matrix de correlação é indicado que o dolar tem correlação inversamente proporcional ao preço do produtor
#Entretanto analisando mais de perto o gráfico pode-se notar que houve um aumento fora do comum no preço do dolar e diminuição do preço do barril
#Fato explicado pelos eventos da Covid no mundo, ontem gerou-se uma crise no qual o barril de petroleo sofreu queda em sua demanda em contra partida
#o dolar sofreu uma alta demanda por liquidez.

#Grafico media gasolina por região vs cotacao dolar
# df<- df%>%filter(variable == 'CotacaoDolar' | variable == 'medianaGasolinaBomba')
# linhaGasolinaBomba <- ggplot(data=df, aes(x = mes, color = Regiao_variable,y = value, group=Regiao_variable)) +
#   geom_line(size=1) +
#   ggtitle("Cotação dólar 2018-07 até 2020-07") +
#   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# linhaGasolinaBomba

str(df_brasil)


ggplot(df_brasil, aes(x=mes)) + 
  geom_line(aes(y = CotacaoDolar), color = "darkred",group=1) + 
  geom_line(aes(y = GasolinaNaBomba), color="steelblue",group=1) + labs(tag = c("Cylinders","a"))


?labs
