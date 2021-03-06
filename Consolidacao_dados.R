setwd("E:/Estudos/Trabalho_Bloco_A")
getwd()

library(dplyr)
library(rJava)
library(readxl)
library(tidyr) #adc por felipe para transpor a coluna em varias colunas
library(ggplot2)
library(Rfast)
library(tidyverse)

#install.packages('Rfast')
#install.packages('quanteda')


#CARREGA IMPOSTOS

df_impostos <-read_excel("ImpostosPrecoGasolina_ConsolidadoTratado.xlsx", sheet = 1)

#transforma data em string

df_impostos$Data<- as.character(df_impostos$Data)

str(df_impostos)

#cria coluna mês
df_impostos$mes<-substr(df_impostos$Data,1,7)

head(df_impostos)

View(df_impostos)

#retira colunas que não serão utilizadas
df_impostos2=df_impostos[-c(1,3,5)]



#---mod felipe
df_impostos3 <- spread(df_impostos2, Taxa, Valor)
options(dplyr.width = Inf)
head(df_impostos3)


#soma os campos Margem Bruta de Distribuição 5 + Custos Transporte e Margem Bruta de Revenda 5 nos registros anteriores a 09/2020
for (i in 1:nrow(df_impostos3))
{
  if (is.na(df_impostos3[i,]$`Margem Bruta de Distribuição + Revenda 5`))
  {
    df_impostos3[i,]$`Margem Bruta de Distribuição + Revenda 5` <- df_impostos3[i,]$`Margem Bruta de Distribuição 5 + Custos Transporte`+df_impostos3[i,]$`Margem Bruta de Revenda 5`
    
  }
}

View(df_impostos3)

#agrupa e gera a media e a mediana

df_impostos3=df_impostos3%>%group_by(Sigla_regiao,mes)%>%
  summarize(mediaPrecoEtanol=mean(`Preço do Etanol Anidro 2`),
            mediaPrecoGasolina=mean(`Preço Produtor de Gasolina A Comum1`),
            mediaTribEstadual=mean(`Tributos Estaduais 4`),
            mediaTribFederal=mean(`Tributos Federais 3`),
            mediaDistRev=mean(`Margem Bruta de Distribuição + Revenda 5`),
            medianaPrecoEtanol=median(`Preço do Etanol Anidro 2`),
            medianaPrecoGasolina=median(`Preço Produtor de Gasolina A Comum1`),
            medianaTribEstadual=median(`Tributos Estaduais 4`),
            medianaTribFederal=median(`Tributos Federais 3`),
            medianaDistRev=median(`Margem Bruta de Distribuição + Revenda 5`)
  )
#----end mod felipe

#tira meses a mais de dados
df_impostos3<-df_impostos3%>%filter(mes<'2020-11')

head(df_impostos3)

View(df_impostos3)

distinct(df_impostos3,Sigla_regiao)


#CARREGA COTAÇÃO DO DOLAR

df_dolar <-read.csv("USD_BRL_Dados_Historicos.csv")

names(df_dolar)[1] <- "DataCotacao"
names(df_dolar)[2] <- "Ultimo"
names(df_dolar)[3] <- "Abertura"
names(df_dolar)[4] <- "Maxima"
names(df_dolar)[5] <- "Minima"
names(df_dolar)[6] <- "Var"

#ajusta a data para ficar igual as outras
df_dolar$DataCotacao <- strptime(df_dolar$DataCotacao, format= "%d.%m.%Y")
format(df_dolar$DataCotacao, format="%Y-%m-%d")

#transforma data em string

df_dolar$DataCotacao<- as.character(df_dolar$DataCotacao)

df_dolar2<-df_dolar[-c(3,4,5,6)]
#retira meses a mais
df_dolar2<-df_dolar2%>%filter(DataCotacao<'2020-11' & DataCotacao>'2018-06')

df_dolar2$Ultimo<- gsub(',', '.', df_dolar2$Ultimo)

df_dolar2$Ultimo<-as.numeric(df_dolar2$Ultimo)


str(df_dolar2)
View(df_dolar2)

#CARREGA VALOR DO BARRIL DE PETRÓLEO

df_brent <-read_excel("BarrilPetroleo/RBRTEd.xls", sheet = 2)

#dropa lixo das primeiras duas linhas
df_brent=df_brent[-c(1,2),]

names(df_brent)[1] <- "DataCotacao"
names(df_brent)[2] <- "ValorBrentEmDolar"

#converte as datas que vieram em dias deste 01/01/1900 
df_brent <- df_brent%>% mutate(DataCotacao = as.Date(as.numeric(as.character(DataCotacao)), origin = "1900-01-01"))

#transforma em data
df_brent$DataCotacao <- strptime(df_brent$DataCotacao, format= "%Y-%m-%d")

df_brent$DataCotacao<- as.character(df_brent$DataCotacao)
df_brent$mes<-substr(df_brent$DataCotacao,1,7)

View(df_brent)

#retira meses a mais
df_brent<-df_brent%>%filter(mes<'2020-11' & mes>'2018-06')

df_brent$ValorBrentEmDolar<-as.numeric(df_brent$ValorBrentEmDolar)


View(df_brent)

#junta dados de barril de petroleo e dolar
df_dolar_brent<-merge(df_brent,df_dolar2,by="DataCotacao")

head(df_dolar_brent)

df_dolar_brent$ValorBrentReais=df_dolar_brent$ValorBrentEmDolar*df_dolar_brent$Ultimo

#retira a coluna de data
df_dolar_brent2=df_dolar_brent[-c(1)]#Alterado Felipe

View(df_dolar_brent2)

names(df_dolar_brent2)[3] <- "CotacaoDolar" #Alterado Felipe
names(df_dolar_brent2)[4] <- "CotacaoBrentReais" #Alterado Felipe
#Alterado Felipe
df_dolar_brent3<-df_dolar_brent2%>%group_by(mes)%>%summarize(CotacaoDolar=mean(CotacaoDolar),CotacaoBrentReais=mean(CotacaoBrentReais),ValorBrentEmDolar=mean(ValorBrentEmDolar))

View(df_dolar_brent3)

#Alteração Felipe -------------------------------------
df_imp_dolar_brent<-merge(df_dolar_brent3,df_impostos3,by="mes")

View(df_imp_dolar_brent)

#CARREGA O PREÇO DA GASOLINA NA BOMBA, não é mais necessário, usamos o valor que vem no daraset de impostos!

df_preco_gas <- data.frame()

df_preco_gas

dir_gasolina="./PrecoGasolina/"

file_list <- list.files(path=dir_gasolina)

file_list

for (i in 1:length(file_list))
{
  print(i)
  temp_data <- read.csv(paste(dir_gasolina,file_list[i],sep = ""),sep = "\t", fileEncoding="UTF-16LE")
  #tira campos inúteis para nossa análise
  temp_data<-temp_data%>%select(1,2,3,Data.da.Coleta,Valor.de.Venda,Valor.de.Compra,Unidade.de.Medida)
  temp_data$Valor.de.Venda<- gsub(',', '.', temp_data$Valor.de.Venda)
  temp_data$Valor.de.Venda=as.numeric(temp_data$Valor.de.Venda)
  temp_data$Valor.de.Compra<- gsub(',', '.', temp_data$Valor.de.Compra)
  temp_data$Valor.de.Compra=as.numeric(temp_data$Valor.de.Compra)
  
  df_preco_gas <- rbind(df_preco_gas, temp_data) #for each iteration, bind the new data to the building dataset
}

View(df_preco_gas)

df_preco_gas$Data.da.Coleta <- strptime(df_preco_gas$Data.da.Coleta, format= "%d/%m/%Y")
format(df_preco_gas$Data.da.Coleta, format="%Y-%m-%d")

df_preco_gas$Data.da.Coleta<- as.character(df_preco_gas$Data.da.Coleta)

names(df_preco_gas)[1] <- "Regiao"
names(df_preco_gas)[2] <- "UF"
names(df_preco_gas)[3] <- "Municipio"
names(df_preco_gas)[4] <- "Data_Coleta"
names(df_preco_gas)[5] <- "Valor_Venda"
names(df_preco_gas)[6] <- "Valor_Compra"
names(df_preco_gas)[7] <- "Unidade_Medida"


str(df_preco_gas)

df_preco_gas$mes<-substr(df_preco_gas$Data_Coleta,1,7)

View(df_preco_gas%>%filter(mes == '2019-08'))

View(df_preco_gas)

sum(is.na (df_preco_gas$Valor_Venda)) 

sum(is.na (df_preco_gas$Valor_Compra))

df_preco_gas2<-df_preco_gas[-c(2,3,4,7)]

View(df_preco_gas2)

df_preco_gas2$Valor_Venda<-as.numeric(df_preco_gas2$Valor_Venda)

df_preco_gas2$Valor_Compra<-as.numeric(df_preco_gas2$Valor_Compra)

df_preco_gas3=df_preco_gas2%>%group_by(Regiao,mes)%>%summarize(mediaValorVendaGas=mean(Valor_Venda),medianaValorVendaGas=median(Valor_Venda),mediaValorCompraGas=mean(Valor_Compra),medianaValorCompraGas=median(Valor_Compra))

View(df_preco_gas3)

View(df_preco_gas3%>%filter(Regiao == 'SE'))


#CARREGA IPCA

df_ipca <-read_excel("ResultadoFinal_IPCA.xlsx")



names(df_ipca)[1] <- "Regiao"
names(df_ipca)[3] <- "Mes_Anterior"
names(df_ipca)[4] <- "IPCA_Mes"
names(df_ipca)[5] <- "Ano"
names(df_ipca)[6] <- "12_meses"
names(df_ipca)[8] <- "Sigla_regiao"

View(df_ipca)

df_ipca$Data <- strptime(df_ipca$Data, format= "%Y-%m-%d")

df_ipca$Data<- as.character(df_ipca$Data)

distinct(df_ipca,Regiao)

df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Aracaju']<- 'NE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'São Luís']<- 'NE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Rio Branco']<- 'N'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Salvador']<- 'NE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Belém']<- 'N'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Rio de Janeiro']<- 'SE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Brasília']<- 'CO'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Porto Alegre']<- 'S'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Recife']<- 'NE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Campo Grande']<- 'CO'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'São Paulo']<- 'SE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Belo Horizonte']<- 'SE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Vitória']<- 'SE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Goiânia']<- 'CO'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Curitiba']<- 'S'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Brasil']<- 'BR'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Fortaleza']<- 'NE'
df_ipca["Sigla_regiao"][df_ipca["Regiao"] == 'Grande Vitória']<- 'SE'

df_ipca$mes<-substr(df_ipca$Data,1,7)

View(df_ipca)

df_ipca%>%filter(mes == '2020-03')%>%summarize(Regiao,PesoRegional)

df_ipca%>%filter(Regiao == 'Vitória')%>%summarize(mes,PesoRegional)


#completa os pesos regionais n�o existentes
df_ipca$PesoRegional[df_ipca$Regiao == "São Paulo" & is.na(df_ipca$PesoRegional)] <-32.3
df_ipca$PesoRegional[df_ipca$Regiao == "Porto Alegre" & is.na(df_ipca$PesoRegional)] <-8.61
df_ipca$PesoRegional[df_ipca$Regiao == "Brasília" & is.na(df_ipca$PesoRegional)] <-4.06
df_ipca$PesoRegional[df_ipca$Regiao == "Belém" & is.na(df_ipca$PesoRegional)] <-3.94
df_ipca$PesoRegional[df_ipca$Regiao == "Rio Branco" & is.na(df_ipca$PesoRegional)] <-0.51
df_ipca$PesoRegional[df_ipca$Regiao == "Belo Horizonte" & is.na(df_ipca$PesoRegional)] <-9.69
df_ipca$PesoRegional[df_ipca$Regiao == "Grande Vitória" & is.na(df_ipca$PesoRegional)] <-1.86
df_ipca$PesoRegional[df_ipca$Regiao == "Goiânia" & is.na(df_ipca$PesoRegional)] <-4.17
df_ipca$PesoRegional[df_ipca$Regiao == "Salvador" & is.na(df_ipca$PesoRegional)] <-5.99
df_ipca$PesoRegional[df_ipca$Regiao == "Fortaleza" & is.na(df_ipca$PesoRegional)] <-3.23
df_ipca$PesoRegional[df_ipca$Regiao == "Recife" & is.na(df_ipca$PesoRegional)] <-3.92
df_ipca$PesoRegional[df_ipca$Regiao == "São Luís" & is.na(df_ipca$PesoRegional)] <-1.62
df_ipca$PesoRegional[df_ipca$Regiao == "Aracaju" & is.na(df_ipca$PesoRegional)] <-1.03
df_ipca$PesoRegional[df_ipca$Regiao == "Rio de Janeiro" & is.na(df_ipca$PesoRegional)] <-9.43
df_ipca$PesoRegional[df_ipca$Regiao == "Campo Grande" & is.na(df_ipca$PesoRegional)] <-1.57
df_ipca$PesoRegional[df_ipca$Regiao == "Curitiba" & is.na(df_ipca$PesoRegional)] <-8.09
df_ipca$PesoRegional[df_ipca$Regiao == "Brasil" & is.na(df_ipca$PesoRegional)] <-32.3

View(df_ipca)

df_ipca2<-df_ipca[-c(1,3,5,6,7)]

View(df_ipca2)

df_ipca3=df_ipca2%>%group_by(Sigla_regiao,PesoRegional,mes)%>%summarize(mediaIpcaMes=mean(IPCA_Mes),medianaIpcaMes=median(IPCA_Mes))

View(df_ipca3)

df_ipca3%>%group_by()%>%summarize(sum(PesoRegional))


df_ipca3%>%distinct(Sigla_regiao)


df_ipca3$Sigla_regiao<- as.character(df_ipca3$Sigla_regiao)

distinct(df_ipca,Regiao)

unique(df_ipca3$Sigla_regiao)

distinct(df_ipca3,Sigla_regiao)

df_ipca4 <- data.frame(matrix(ncol = 4, nrow = 0))

names(df_ipca4)[1] <- "Sigla_regiao"
names(df_ipca4)[2] <- "mes"
names(df_ipca4)[3] <- "MediaIPCAMes"
names(df_ipca4)[4] <- "MedianaIPCAMes"


str(df_ipca4)

for (reg in unique(df_ipca3$Sigla_regiao))
{
   for (month in unique(df_ipca3$mes) )
   {
      
      df_tmp<-df_ipca3%>%filter(mes == month & Sigla_regiao == reg)
      
      PesoTotal<-df_tmp%>%group_by()%>%summarize(sum(PesoRegional))
      
      SomaMediaPonderada=0
      SomaMedianaPonderada=0
      
      for (i in 1:nrow(df_tmp))
      {
         SomaMediaPonderada=df_tmp[i,]$PesoRegional*df_tmp[i,]$mediaIpcaMes
         SomaMedianaPonderada=df_tmp[i,]$PesoRegional*df_tmp[i,]$medianaIpcaMes
         
      }
      
      df_tmp2 <- data.frame(reg,month,SomaMediaPonderada/PesoTotal,SomaMedianaPonderada/PesoTotal)
      
      #tratamento para primeira linha não alterar nome das colunas
      df_ipca4 <- rbind(df_ipca4,setNames(df_tmp2, names(df_ipca4)))
      
      #df_ipca4 <- rbind(df_ipca4,c(Sigla_regiao,mes,mediaIpcaMes,medianaIpcaMes))
      
      # x=sprintf("%s,%s",reg,month)
      # print(x)
   }
}


View(df_ipca4)

str(df_ipca4)


#cria dataframe final
df_final <- df_imp_dolar_brent %>% inner_join(df_ipca4, by=c("mes","Sigla_regiao"))



View(df_final)


write.csv(df_final, "df_final.csv",row.names=FALSE)

#carrega dataframe final

df_total <- read.csv("df_final.csv")

str(df_total)

#df_total=df_total[-1]

#calcula preço médio da gasolina na bomba
df_total<-df_total %>%
  mutate(mediaGasolinaBomba = select(.,6:10) %>% rowSums(na.rm = TRUE))

#calcula percetagem do imposto 

df_total$ImpPercMedia=df_total$mediaTribEstadual*100/df_total$mediaGasolinaBomba     


#calcula preço por mediana da gasolina na bomba
df_total<-df_total %>%
  mutate(medianaGasolinaBomba = select(.,11:15) %>% rowSums(na.rm = TRUE))


#calcula percetagem do imposto 

df_total$ImpPercMediana=df_total$medianaTribEstadual*100/df_total$medianaGasolinaBomba  



View(df_total)





write.csv(df_total, "df_final.csv",row.names=FALSE)

#novos dados
     

#PIM

df_pim_pf <-read_excel("PIM_PF.xlsx", sheet = 1)

View(df_pim_pf)


#dropa lixo das primeiras 3 linhas
df_pim_pf=df_pim_pf[-c(1,2,3),]

df_pim_pf=df_pim_pf[-c(29),]

df_pim_pf=df_pim_pf[-c(2)]

names(df_pim_pf)[1] <- "Data"
names(df_pim_pf)[2] <- "PIM"


#trata a data para ser igual as outras
df_pim_pf<-df_pim_pf %>% 
  mutate(Data = str_replace_all(Data, c("janeiro" = "01 01","fevereiro" = "01 02","março" = "01 03","abril" = "01 04","maio"="01 05","junho"="01 06","julho"="01 07","agosto"="01 08","setembro"="01 09","outubro"="01 10","novembro"="01 11","dezembro"="01 12")))


df_pim_pf$Data <- strptime(df_pim_pf$Data, format= "%d %m %Y")

format(df_pim_pf$Data, format="%Y-%m-%d")

df_pim_pf$Data<- as.character(df_pim_pf$Data)

df_pim_pf$mes<-substr(df_pim_pf$Data,1,7)

df_pim_pf=df_pim_pf[-c(1)]

df_total2 <- df_total %>% inner_join(df_pim_pf, by=c("mes"))

View(df_total2)


#Estoque de Empregos Formais

df_EEF <- read.csv2("Estoque_Empregos_Formais.csv")

View(df_EEF)

df_EEF=df_EEF[-c(29),]


names(df_EEF)[2] <- "Estoque_Empregos"

df_EEF$Data <- strptime(paste('01',df_EEF$Data, sep="/"), format= "%d/%m/%Y")

format(df_EEF$Data, format="%Y-%m-%d")

df_EEF$Data<- as.character(df_EEF$Data)

df_EEF$mes<-substr(df_EEF$Data,1,7)

df_EEF=df_EEF[-c(1)]

df_total3 <- df_total2 %>% inner_join(df_EEF, by=c("mes"))

View(df_total3)


#CDI

df_cdi <-read.csv("CDI.csv",sep = ";",dec = ",")

View(df_cdi)

names(df_cdi)[2] <- "N.Operacoes"
names(df_cdi)[4] <- "Media"
names(df_cdi)[5] <- "Fator.Diario"


df_cdi$Data <- strptime(df_cdi$Data, format= "%d/%m/%Y")
format(df_cdi$Data, format="%Y-%m-%d")

df_cdi$Data<- as.character(df_cdi$Data)

df_cdi$mes<-substr(df_cdi$Data,1,7)

df_cdi2=df_cdi[-c(1,2,3,5,6,7,8,9,10)]

View(df_cdi2)


df_cdi3=df_cdi2%>%group_by(mes)%>%summarize(mediaCDI=mean(Media))

View(df_cdi3)

df_total4 <- df_total3 %>% inner_join(df_cdi3, by=c("mes"))

View(df_total4)

#IBC

df_ibc<-read.csv("IBC.csv",sep = ";",dec = ",")

View(df_ibc)

df_ibc=df_ibc[-c(29),]

names(df_ibc)[2] <- "IBC"

df_ibc$Data <- strptime(paste('01',df_ibc$Data, sep="/"), format= "%d/%m/%Y")

format(df_ibc$Data, format="%Y-%m-%d")

df_ibc$Data<- as.character(df_ibc$Data)

df_ibc$mes<-substr(df_ibc$Data,1,7)

df_ibc=df_ibc[-c(1)]

df_total5 <- df_total4 %>% inner_join(df_ibc, by=c("mes"))

View(df_total5)

write.csv(df_total5, "df_final.csv",row.names=FALSE)

#Massa de rendimento

df_total5 <- read.csv("df_final.csv")

View(df_total5)

df_mr <-read_excel("MassaRendimento.xlsx", sheet = 1)

View(df_mr)

df_mr=df_mr[-c(1,2,5),]


df_mr_transpose <- as.data.frame(t(as.matrix(df_mr)))

View(df_mr_transpose)


names(df_mr_transpose)[1] <- "Data"
names(df_mr_transpose)[2] <- "MassaRendimento"

df_mr_transpose=df_mr_transpose[-c(1),]


str(df_mr_transpose)

head(df_mr_transpose)

df_mr_transpose$Data2


strsplit(as.character(df_mr_transpose$Data[1]), "-")[[1]][3]

?strsplit


df_mr_transpose$Data2<-lapply(df_mr_transpose$Data, function(x){
  #mean.pl <- mean(x$petal.length)
  Data2<-strsplit(as.character(x), "-")[[1]][3]
})

df_mr_transpose<-df_mr_transpose %>% 
  mutate(Data2= str_replace_all(Data2, c("jan" = "01 01","fev" = "01 02","mar" = "01 03","abr" = "01 04","mai"="01 05","jun"="01 06","jul"="01 07","ago"="01 08","set"="01 09","out"="01 10","nov"="01 11","dez"="01 12")))

df_mr_transpose$Data2 <- strptime(df_mr_transpose$Data2, format= "%d %m %Y")

format(df_mr_transpose$Data2, format="%Y-%m-%d")

df_mr_transpose$Data2<- as.character(df_mr_transpose$Data2)

df_mr_transpose$mes<-substr(df_mr_transpose$Data2,1,7)

df_mr_transpose=df_mr_transpose[-c(1,3)]

df_total6 <- df_total5 %>% inner_join(df_mr_transpose, by=c("mes"))

View(df_total6)


write.csv(df_total6, "df_final.csv",row.names=FALSE)

#Seta dados do Brasil

df_total <- read.csv("df_final.csv")
View(df_total)

str(df_total)

#mantem os valores de média
df_media=df_total[c(1,2,3,4,5,6,7,8,9,10,16,19,22,23,24,25,26,18)]

str(df_media)

View(df_media)

#Mantém os valores de mediana
df_mediana=df_total[c(1,2,3,4,5,11,12,13,14,15,17,21,22,23,24,25,26,20)]
str(df_mediana)


View(df_mediana)


df_mediana=df_mediana[-c(4)]


names(df_mediana)[2] <- "Dolar"
names(df_mediana)[3] <- "CtBarril"

names(df_mediana)[5] <- "Etanol"
names(df_mediana)[6] <- "GasolProdr"
names(df_mediana)[7] <- "TribEst"
names(df_mediana)[8] <- "TribFed"
names(df_mediana)[9] <- "DistrRev"
names(df_mediana)[10] <- "IPCA"
names(df_mediana)[11] <- "PCTribEst"
names(df_mediana)[13] <- "EstEmp"
names(df_mediana)[14] <- "CDI"
names(df_mediana)[16] <- "MRed"
names(df_mediana)[17] <- "GasolNaBomba"

df_mediana$IBC = as.numeric(gsub(",", ".", df_mediana$IBC ))

head(df_mediana) 

str(df_mediana)

df_mediana$EstEmp = gsub("\\.","",df_mediana$EstEmp)

head(df_mediana) 


df_mediana$EstEmp = as.numeric(gsub(",",".",df_mediana$EstEmp))

View(df_mediana)


write.csv(df_mediana, "df_todas_regioes.csv",row.names=FALSE)




#BRASIL
df_brasil <- subset(df_mediana, Sigla_regiao== 'BR')

df_brasil=df_brasil[-c(4)]

View(df_brasil)

str(df_brasil)


write.csv(df_brasil, "df_brasil.csv",row.names=FALSE)

#SUDESTE
df_sudeste <- subset(df_mediana, Sigla_regiao== 'SE')

df_sudeste=df_sudeste[-c(4)]

View(df_sudeste)

str(df_sudeste)


write.csv(df_sudeste, "df_sudeste.csv",row.names=FALSE)

#SUL
df_sul <- subset(df_mediana, Sigla_regiao== 'S')

df_sul=df_sul[-c(4)]

View(df_sul)

str(df_sul)


write.csv(df_sul, "df_sul.csv",row.names=FALSE)

#CENTRO OESTE
df_centro_oeste <- subset(df_mediana, Sigla_regiao== 'CO')

df_centro_oeste=df_centro_oeste[-c(4)]

View(df_centro_oeste)

str(df_centro_oeste)


write.csv(df_centro_oeste, "df_centro_oeste.csv",row.names=FALSE)

#NORDESTE
df_nordeste <- subset(df_mediana, Sigla_regiao== 'NE')

df_nordeste=df_nordeste[-c(4)]

View(df_nordeste)

str(df_nordeste)


write.csv(df_nordeste, "df_nordeste.csv",row.names=FALSE)

#NORTE
df_norte <- subset(df_mediana, Sigla_regiao== 'N')

df_norte=df_norte[-c(4)]

View(df_norte)

str(df_norte)


write.csv(df_norte, "df_norte.csv",row.names=FALSE)

summary(df_brasil, maxsum = max(lengths(lapply(df_brasil, unique))))


