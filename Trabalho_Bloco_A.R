setwd("C:/Users/felip/OneDrive/feliped16/OneDrive/PosGraduação BigData/Bloco A - Analítico e estudo de caso/Trabalho Petroleo-Gasolina")
getwd()

library(dplyr)
library(rJava)
library(readxl)
library(tidyr) #adc por felipe para transpor a coluna em varias colunas


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
#agrupa e gera a media e a mediana

df_impostos3=df_impostos3%>%group_by(Sigla_regiao,mes)%>%
  summarize(mediaDistrTransporte=mean(`Margem Bruta de Distribuição 5 + Custos Transporte`), 
            mediaRevenda=mean(`Margem Bruta de Revenda 5`),
            mediaPrecoEtanol=mean(`Preço do Etanol Anidro 2`),
            mediaPrecoGasolina=mean(`Preço Produtor de Gasolina A Comum1`),
            mediaTribEstadual=mean(`Tributos Estaduais 4`),
            mediaTribFederal=mean(`Tributos Federais 3`),
            medianaDistrTransporte=median(`Margem Bruta de Distribuição 5 + Custos Transporte`), 
            medianaRevenda=median(`Margem Bruta de Revenda 5`),
            medianaPrecoEtanol=median(`Preço do Etanol Anidro 2`),
            medianaPrecoGasolina=median(`Preço Produtor de Gasolina A Comum1`),
            medianaTribEstadual=median(`Tributos Estaduais 4`),
            medianaTribFederal=median(`Tributos Federais 3`)
  )
#----end mod felipe

#tira meses a mais de dados
df_impostos3<-df_impostos3%>%filter(mes<'2020-08')

head(df_impostos3)

View(df_impostos3)

distinct(df_impostos3,Sigla_regiao)

#CARREGA COTAÇÃO DO DOLAR

df_dolar <-read.csv("CotaçãoDolar/USD_BRL_Dados_Historicos.csv")

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
df_dolar2<-df_dolar2%>%filter(DataCotacao<'2020-08' & DataCotacao>'2018-06')

df_dolar2$Ultimo<- gsub(',', '.', df_dolar2$Ultimo)

df_dolar2$Ultimo<-as.numeric(df_dolar2$Ultimo)


str(df_dolar2)
View(df_dolar2)

#CARREGA VALOR DO BARRIL DE PETRÓLEO

df_brent <-read_excel("BarrilPetroleo/RBRTEd.xls", sheet = 2)

#dropa lixo das primeiras duas linhas
df_brent=df_brent[-c(1,2),]

names(df_brent)[1] <- "DataCotacao"
names(df_brent)[2] <- "ValorEmDolar"

#converte as datas que vieram em dias deste 01/01/1900 
df_brent <- df_brent%>% mutate(DataCotacao = as.Date(as.numeric(as.character(DataCotacao)), origin = "1900-01-01"))

#transforma em data
df_brent$DataCotacao <- strptime(df_brent$DataCotacao, format= "%Y-%m-%d")

df_brent$DataCotacao<- as.character(df_brent$DataCotacao)
df_brent$mes<-substr(df_brent$DataCotacao,1,7)

View(df_brent)

#retira meses a mais
df_brent<-df_brent%>%filter(mes<'2020-08' & mes>'2018-06')

df_brent$ValorEmDolar<-as.numeric(df_brent$ValorEmDolar)


View(df_brent)

#junta dados de barril de petroleo e dolar
df_dolar_brent<-merge(df_brent,df_dolar2,by="DataCotacao")

head(df_dolar_brent)

df_dolar_brent$ValorBrentReais=df_dolar_brent$ValorEmDolar*df_dolar_brent$Ultimo

#retira a coluna de data
df_dolar_brent2=df_dolar_brent[-c(1)]#Alterado Felipe

View(df_dolar_brent2)

names(df_dolar_brent2)[3] <- "CotacaoDolar" #Alterado Felipe
names(df_dolar_brent2)[4] <- "CotacaoBrentReais" #Alterado Felipe
#Alterado Felipe
df_dolar_brent3<-df_dolar_brent2%>%group_by(mes)%>%summarize(CotacaoDolar=mean(CotacaoDolar),CotacaoBrentReais=mean(CotacaoBrentReais),ValorEmDolar=mean(ValorEmDolar))

View(df_dolar_brent3)

#Alteração Felipe -------------------------------------
df_imp_dolar_brent<-merge(df_dolar_brent3,df_impostos3,by="mes")



#Fim alteração Felipe ------------------


#CARREGA O PREÇO DA GASOLINA NA BOMBA

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
names(df_ipca)[8] <- "Sigla_Regiao"

View(df_ipca)

df_ipca$Data <- strptime(df_ipca$Data, format= "%Y-%m-%d")

df_ipca$Data<- as.character(df_ipca$Data)

distinct(df_ipca,Regiao)

df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Aracaju']<- 'NE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'São Luís']<- 'NE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Rio Branco']<- 'N'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Salvador']<- 'NE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Belém']<- 'N'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Rio de Janeiro']<- 'SE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Brasília']<- 'CO'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Porto Alegre']<- 'S'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Recife']<- 'NE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Campo Grande']<- 'CO'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'São Paulo']<- 'SE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Belo Horizonte']<- 'SE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Vitória']<- 'SE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Goiânia']<- 'CO'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Curitiba']<- 'S'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Brasil']<- 'BR'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Fortaleza']<- 'NE'
df_ipca["Sigla_Regiao"][df_ipca["Regiao"] == 'Grande Vitória']<- 'SE'

df_ipca$mes<-substr(df_ipca$Data,1,7)

View(df_ipca)

df_ipca2<-df_ipca[-c(1,3,5,6,7)]

View(df_ipca2)

df_ipca3=df_ipca2%>%group_by(Sigla_Regiao,PesoRegional,mes)%>%summarize(mediaIpcaMes=mean(IPCA_Mes),medianaIpcaMes=median(IPCA_Mes))

View(df_ipca3)
