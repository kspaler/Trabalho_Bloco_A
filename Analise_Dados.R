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


df_total <- read.csv("df_final.csv")
View(df_total)

str(df_total)

#mantem os valores de média
df_media=df_total[c(1,2,3,4,5,6,7,8,9,10,11,18,22,23,24,20)]

str(df_media)

View(df_media)

#Mantém os valores de mediana
df_mediana=df_total[c(1,2,3,4,5,12,13,14,15,16,17,19,22,23,24,25,21)]
str(df_mediana)


View(df_mediana)


#serão utilizadas as medianas nas nossas análises, e será analisado somente o Brasil
df_brasil <- subset(df_mediana, Sigla_regiao== 'BR')

df_brasil=df_brasil[-c(4,5)]


str(df_brasil)
names(df_brasil)[3] <- "CotacaoBarrilPetroleo"
names(df_brasil)[4] <- "Distrib_trans"
names(df_brasil)[5] <- "Revenda"
names(df_brasil)[6] <- "Etanol"
names(df_brasil)[7] <- "GasolinaProdutor"
names(df_brasil)[8] <- "TribEstaduais"
names(df_brasil)[9] <- "TribFederais"
names(df_brasil)[10] <- "IPCA"
names(df_brasil)[13] <- "CDI"
names(df_brasil)[15] <- "GasolinaNaBomba"

df_brasil$Estoque_Empregos=as.numeric(df_brasil$Estoque_Empregos)

df_brasil$IBC=as.numeric(df_brasil$IBC)


#verificando linhas duplicadas
sum(duplicated(df_brasil))

#verifica se tem registros nulos
sum(is.na(df_brasil))

#Estatísticas básicas: mínimos, máximos, médias, desvios, etc. de cada variável;
summary(df_brasil, maxsum = max(lengths(lapply(df_total, unique))))

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


corrplot(cor(df_brasil_cor), method = "number", type = "lower")

corrplot(cor(df_brasil_cor), method = "circle", type = "lower")

#Explica númericamente a correlação
cor(df_brasil_cor,method = "pearson")

cor(df_brasil_cor,method = "spearman")


#Podemos observar que há correlação do preço do produtor com o preço do barril de petróleo e a cotação do dolar
#Abaixo será plotado a curva do brent, cotacao dolar e o preço do produtor

#Gráfico da variação do dolar e do barril de petroleo (brent)
linhaDolar <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = CotacaoDolar, group=1), color = "darkred", size=2)+
  ggtitle("Cotação dólar 2018-07 até 2020-07") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaBrent <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = CotacaoBarrilPetroleoBrent, group=2), color = "steelblue", size=2)+
  ggtitle("Cotação Barril em Real 2018-07 até 2020-07") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))  


#gráfico da variação do preço da gasolina pelo produtor
linhaVarProdutor <- ggplot(data=df_brasil, aes(x=mes)) +
  geom_line(aes(y = mediaPrecoGasolina, group=1), color = "darkred", size=2)+
  ggtitle("Preço produtor gasolina 2018-07 até 2020-07") +
  ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaDolar+linhaBrent+linhaVarProdutor

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
