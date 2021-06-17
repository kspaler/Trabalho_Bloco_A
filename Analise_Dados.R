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
library(caret)





df_total <- read.csv("df_todas_regioes.csv")


df_brasil <- read.csv("df_brasil.csv")


df_sudeste <- read.csv("df_sudeste.csv")


df_sul <- read.csv("df_sul.csv")


df_centro_oeste <- read.csv("df_centro_oeste.csv")


df_nordeste <- read.csv("df_nordeste.csv")


df_norte <- read.csv("df_norte.csv")


#verificando linhas duplicadas
sum(duplicated(df_total))

#verifica se tem registros nulos
sum(is.na(df_total))


geraAnalise <- function(df,name) 
{ 
  
  #Estatisticas descritivas
  summname=sprintf("%s.txt",name)
  
  write.table(summary(df, maxsum = max(lengths(lapply(df, unique)))), file = summname, sep=';')
  

  
  
  #correlação
  df_cor <- df
  
  #converter para numérico
  for(i in 1:ncol(df_cor)){
    
    df_cor[,i]<- as.numeric(df_cor[,i])
  }
  
  #correlação pearson
   pearsontitle=sprintf("Correlação por Pearson - %s",name)
   pearsonname=sprintf("pearson_%s.png",name)
   png(pearsonname, width = 600, height = 600, res = 72)
  
  
   corrplot(cor(df_cor[-1],method = "pearson"), method = "number", type = "lower",title=pearsontitle,mar=c(0,0,1,0))
  
   dev.off()
  #correlação spearman
  
   spearmantitle=sprintf("Correlação por Spearman - %s",name)
   spearmanname=sprintf("spearman_%s.png",name)
   png(spearmanname, width = 600, height = 600, res = 72)
  
   corrplot(cor(df_cor[-1],method = "spearman"), method = "number", type = "lower",title=spearmantitle,mar=c(0,0,1,0))
  
   dev.off()
  
  
  #análise exploratória das principais variáveis
  
  #preço do dolar versus o preço da gasolina
  
  df_dl_gas <- df %>%
    tidyr::gather(objeto, reais,c(Dolar,GasolNaBomba))
  
  # str(df_dl_gas)
  # 
  file_dolarxgasol=sprintf("dolar_x_gasol_%s.png",name)

  ggplot(df_dl_gas, aes(x = mes, y = reais, color = objeto,group=objeto)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Dolar x Gasolina na bomba", subtitle = name )
  ggsave(file=file_dolarxgasol)

  #Gráfico da Cotação do barril em reais


  file_brent=sprintf("brent_%s.png",name)
  
  ggplot(data=df, aes(x=mes)) +
     geom_line(aes(y = CtBarril, group=2), color = "steelblue", size=1)+
     ggtitle("Cotação Barril em Real", subtitle = name) +
     ylab("Cotação (R$)")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file=file_brent)

  #gráfico da variação da alíquota de imposto

   file_imp=sprintf("Imp_%s.png",name)
   
   ggplot(data=df, aes(x=mes)) +
     geom_line(aes(y = PCTribEst, group=1), color = "darkred", size=1)+
     ggtitle("Alíquota impostos estaduais", subtitle = name) +
     ylab("%")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
   ggsave(file=file_imp)
   
   
   #preço do tributo federal versus o preço da gasolina
   
   df_if_gas <- df %>%
     tidyr::gather(objeto, reais,c(TribFed,GasolNaBomba))

   file_trfedxgasol=sprintf("TribFed_x_gasol_%s.png",name)

   ggplot(df_if_gas, aes(x = mes, y = reais, color = objeto,fill=objeto)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Tributo Federal x Gasolina na bomba", subtitle = name)
   ggsave(file=file_trfedxgasol)
   
   
   #Min-max Scaling
   
   preproc <- preProcess(df[,c(3,10,16)], method=c("range"))
   
   norm <- predict(preproc, df[,c(3,10,16)])
   norm$mes<-df$mes
   
   
   #Barril de petróleo x Gasolina na bomba normalizados
   df_bar_gas <- norm %>%
     tidyr::gather(objeto, Valor,c(CtBarril,GasolNaBomba))  
   
   file_bargasol=sprintf("barril_x_gasol_%s.png",name)
   
   ggplot(df_bar_gas, aes(x = mes, y = Valor, color = objeto,group=objeto)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Barril de Petroleo x Gasolina na bomba (normalizados)", subtitle = name)
   ggsave(file=file_bargasol)
   
   #Imposto Estadual x Gasolina na bomba normalizados
   df_tes_gas <- norm %>%
     tidyr::gather(objeto, Valor,c(PCTribEst,GasolNaBomba))  
   
   file_tesgasol=sprintf("TribEst_x_gasol_%s.png",name)
   
   ggplot(df_tes_gas, aes(x = mes, y = Valor, color = objeto,group=objeto)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Tributo Estadual x Gasolina na bomba (normalizados)", subtitle = name)
   ggsave(file=file_tesgasol)
   
}

geraAnalise(df_brasil,"Brasil")

geraAnalise(df_sudeste,"Sudeste")

geraAnalise(df_sul,"Sul")

geraAnalise(df_centro_oeste,"Centro Oeste")

geraAnalise(df_nordeste,"Nordeste")

geraAnalise(df_norte,"Norte")



df_if_gas <- df_brasil %>%
   tidyr::gather(objeto, reais,c(TribFed,GasolNaBomba))

ggplot(df_if_gas, aes(x = mes, y = reais, color = objeto,fill=objeto)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Tributo Federal x Gasolina na bomba", subtitle = "Brasil")


