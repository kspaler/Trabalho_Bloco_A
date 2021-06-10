setwd('D:\\Trabalho_Bloco_A')
 

#Bibliotecas 
library(tseries)
library(tidyverse)
 
#importando o dataframe

df_final <-read.csv('df_final.csv')


#tratando numeros

df_final$IBC = as.numeric(gsub(",", ".", df_final$IBC ))


df_final$Estoque_Empregos = gsub("\\,","",df_final$Estoque_Empregos)
df_final$Estoque_Empregos = as.numeric(gsub("\\.","",df_final$Estoque_Empregos))

#filtrando df para BR
df<- df_final  %>% filter(Sigla_regiao == "BR")

#separando em treino e Teste
training <- df[1:20,]  
test <- df[21:25,] 

#variaveis
y <- ts(cbind(training[,"mediaGasolinaBomba"]))
d.y <- diff(y) #diferenca

x <- subset(training, select = cbind('IBC', 'PIM', 'CotacaoBrentReais', 'CotacaoDolar','MedianaIPCAMes','mediaCDI', 'Estoque_Empregos')) 
t <- training$mes


#testando  estacionariedade  
plot.ts(y)
plot.ts(d.y)


# Dickey-Fuller test for variable
adf.test(y, alternative="stationary", k=0)
adf.test(y, alternative="explosive", k=0)

summary(lm(dppi ~ lppi, na.action=na.omit))
summary(lm(dppi ~ lppi + trend, na.action=na.omit))

# Augmented Dickey-Fuller test
adf.test(y, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.y, k=0)
adf.test(d.y)


# ACF and PACF
acf(y) #mostra
pacf(y)

acf(d.y)
pacf(d.y)


fit <- arima(y, xreg=x, order=c(1,1,0))
fit

training <- subset(df_final, end=length(auscafe)-61)
test <- subset(df_final, start=length(auscafe)-60)
cafe.train <- Arima(training, order=c(2,1,1),
                    seasonal=c(0,1,2), lambda=0)
cafe.train %>%
  forecast(h=60) %>%
  autoplot() + autolayer(test)



autoplot(training, series="Training data") +
  autolayer(fitted(cafe.train, h=12),
            series="12-step fitted values")


(fit <- auto.arima(d.y,
                   xreg=x))