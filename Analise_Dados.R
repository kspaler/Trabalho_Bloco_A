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


df_total <- read.csv("df_final.csv")
View(df_total)

str(df_total)

#Tira os valores de média
df_media=df_total[c(2,6,3,4,5,7,8,9,10,11,12,19,21)]

str(df_media)

View(df_media)

#Tira os valores de mediana
df_mediana=df_total[c(2,6,3,4,5,13,14,15,16,17,18,20,22)]
str(df_mediana)


View(df_mediana)


#verificando linhas duplicadas
sum(duplicated(df_total))

#verifica se tem registros nulos
sum(is.na(df_total))

#Estatísticas básicas: mínimos, máximos, médias, desvios, etc. de cada variável;
summary(df_total, maxsum = max(lengths(lapply(df_total, unique))))

#Análise utilizando a média


#plotar a relação de cada uma das varíaveis em relação ao preço da bom,ba da gasolina


# plot


colors()

#um tipo de plot múltiplo
ggplot(subset(df_media, Sigla_regiao== 'BR'), aes(x=mes)) + 
   geom_line(aes(y = CotacaoDolar), color = "darkred",group = 1) +
   geom_line(aes(y = CotacaoBrentReais  ), color="steelblue",group = 1) +
   geom_line(aes(y = mediaDistrTransporte  ), color="seagreen",group = 1) +
   geom_line(aes(y = mediaRevenda  ), color="purple",group = 1) +
   geom_line(aes(y = mediaPrecoEtanol  ), color="pink1",group = 1) +
   geom_line(aes(y = mediaPrecoGasolina  ), color="maroon",group = 1) + 
   geom_line(aes(y = mediaTribEstadual  ), color="honeydew2",group = 1) +
   geom_line(aes(y = mediaTribFederal  ), color="grey22",group = 1) +
   geom_line(aes(y = MediaIPCAMes  ), color="aquamarine",group = 1) +
   geom_line(aes(y = mediaGasolinaBomba  ), color="brown4",group = 1)


#outro tipo de plot múltiplo
df <- subset(df_media, Sigla_regiao== 'BR') %>%
   select(mes, CotacaoDolar, CotacaoBrentReais,mediaDistrTransporte,mediaRevenda,mediaPrecoEtanol,mediaPrecoGasolina,mediaTribEstadual,mediaTribEstadual,mediaTribFederal,MediaIPCAMes,mediaGasolinaBomba) %>%
   gather(key = "variable", value = "value", -mes)

View(df)

ggplot(df, aes(x = mes, y = value)) + 
   geom_line(aes(color = variable, size=2)) + 
   scale_color_manual(values = c("darkred", "steelblue","seagreen","purple","pink1","maroon","honeydew2","grey22","aquamarine","brown4"))



View(df)

?gather

plot()

#Primeiramente vou gerar um novo dataset filtrando apenas o Brasil para analise (sem levar em consideração por região)
df_total_brasil <- subset(df_total, Sigla_regiao== 'BR')

View(df_total_brasil)

str(df_total_brasil)


#Após feito a junção dos dados em um único dataset, será plotado algumas visões

#Gráfico da variação do dolar e do barril de petroleo (brent)
linhaDolar <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = CotacaoDolar, group=1), color = "darkred", size=2)+
   ggtitle("Cotação dólar 2018-07 até 2020-07") +
   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaBrent <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = CotacaoBrentReais, group=2), color = "steelblue", size=2)+
   ggtitle("Cotação Barril em Real 2018-07 até 2020-07") +
   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))  

linhaDolar+linhaBrent


#gráfico da variação do preço da gasolina pelo produtor
linhaVarProdutor <- ggplot(data=df_total_brasil, aes(x=mes)) +
   geom_line(aes(y = mediaPrecoGasolina, group=1), color = "darkred", size=2)+
   ggtitle("Preço produtor gasolina 2018-07 até 2020-07") +
   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

linhaDolar+linhaBrent+linhaVarProdutor


str(df_media)

linhaGasolinaBomba <- ggplot(data=df_media, aes(x=mes, color = Sigla_regiao,y = mediaGasolinaBomba, group=Sigla_regiao)) +
   geom_line(size=1) +
   ggtitle("Cotação dólar 2018-07 até 2020-07") +
   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
   

linhaGasolinaBomba

View(df_media)


linhaGasolinaBomba <- ggplot(data=df_media, aes(x=mes, color = Sigla_regiao,y = mediaGasolinaBomba, group=Sigla_regiao)) +
   geom_line(size=1) +
   ggtitle("Cotação dólar 2018-07 até 2020-07") +
   ylab("Cotação")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   geom_line(aes(y = CotacaoDolar), color = "darkred",group = 1,size=2)

linhaGasolinaBomba



