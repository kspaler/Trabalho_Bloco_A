setwd('D:\\Trabalho_Bloco_A')
 
#Bibliotecas 
library(tseries)
library(tidyverse)
library(urca)
library(forecast)
library(smooth)
require(Mcomp)
library(lmtest)
 
library(MLmetrics)#Avaliacao do Modelo

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

#Leitura e divisão de dados

#importando o dataframe
df_final <-read.csv('df_brasil.csv')
df_final=df_final[-c(1,6,7)]



#plotando variaveis
df_ts<-ts(df_final, start = c(2018,07), end=c(2020,10), freq=12)  
autoplot( df_ts, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Variáveis economicas e o Preço da Gasolina")

# divisão treino e teste

#separando em treino e Teste
training <- df_final[1:24,]  #c(2018,07), end=c(2020,06)
test <- df_final[25:28,]     #start = c(2020,07), end=c(2020,10)




#variavel endogena
y_train <- ts(as.vector(training[, c("GasolNaBomba")]))
y_train <- ts(y_train, start =c(2018,07), end=c(2020,06), freq=12)  

y_test <- ts(as.vector(test[, c("GasolNaBomba")]))
y_test <- ts(y_test, start = c(2020,07), end=c(2020,10), freq=12)  

training

#variavel exogena
x_train <- training  %>% select(Dolar,CtBarril,Etanol,GasolProdr,PIM, IPCA,CDI, EstEmp,DistrRev,MRed,IBC)
x_train <- ts(as.vector(x_train))
x_train <- ts(x_train, start = c(2018,07), end=c(2020,06), freq=12)  

x_test <- subset(test, select = cbind('Dolar','CtBarril','Etanol','GasolProdr','PIM', 'IPCA','CDI', 'EstEmp','DistrRev','MRed','IBC')) 
x_test <- ts(as.vector(x_test))
x_test <- ts(x_test,  start = c(2020,07), end=c(2020,10), freq=12) 



################################################################################################################

#### 1.Modelo naive simples (BASE LINE)

naive = snaive(y_train, h=4)

#avaliacao
MAPE(naive$mean, y_test) * 100 #  1.57% de erro

#plot
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)



################################################################################################################
  
  
####  2 . MEDIA MOVEL


#media simples 
m0 <- mean(y_train)
f_mean <- ts(rep(m0, each=4),start =c(2020,07), end=c(2020,10), freq=12)  
MAPE(f_mean, y_test) * 100 # 2.52% de erro

plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Media Simples", type='l')
lines(f_mean, col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)



#iniciando a variavel
m <- 0
m[0] <- MAPE(f_mean, y_test) * 100 # 

for(k in 1:12) { 
  print(cat("modelo ",k,"\n"))
  print(MAPE(forecast(sma(y_train ,order = k ,h=4, silent=FALSE),h = 4)$mean,y_test)*100) 
  m[k] <-MAPE(forecast(sma(y_train ,order = k ,h=4, silent=FALSE),h = 4)$mean,y_test)*100 
}

 
# melhor modelo SMA(12)  MAPE 1.352%
m12 <-  forecast(sma(y_train ,order = 12 ,h=4, silent=FALSE),h = 4) 
plot(m12)
 

#plot FIT
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Modelo média móvel Doze periodos - Fit(vermelho)", type='l')
lines(sma(y_train ,order = 12 , silent=FALSE), col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)

#plot PREVISAO
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Modelo média móvel Doze periodos", type='l')
lines(m12$mean, col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)
MAPE(forecast(sma(y_train ,order = 12 ,h=4, silent=FALSE),h = 4)$mean,y_test)*100  # MAPE 1.35%




########################################################################################################################



####  3 Regressão Linear Múltipla



regressao=step(lm(GasolNaBomba~. , data = training),direction = 'backward')
summary(regressao)

#modelo com coeficientes significativos  sem o preco do produtor que estava deixando R2 1

regressao_2 = step(lm(GasolNaBomba~Dolar+CtBarril + PCTribEst +Etanol+ IPCA+EstEmp + CDI+
                        IBC, data = training),direction = 'backward') #Create the linear regression
summary(regressao_2)


#avaliacao 
lm_forecast = forecast(regressao_2,  h = 4)
MAPE(lm_forecast[1:4],  test$GasolNaBomba) *100   # % erro   #5.036% 

y_heat<-predict(regressao_2, newdata = test[ , names(test) != "GasolNaBomba"]) 


y_heat <- ts(y_heat, start = c(2018,07), end=c(2020,10), freq=12)  
y_2 <- ts(regressao_2$fitted.values, start = c(2018,07), end=c(2020,10), freq=12)  
#plot Fit
plot(training[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Modelo Regressão Linear Múltipla - Fitted", type='l')
lines(regressao_2$fitted.values,  col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)
#plot previsao
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Modelo Regressão Linear Múltipla - Previsto vs Realizado", type='l')
lines(y_2,  col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)

########################################################################################################################

#########  Séries Temporais

####  4.1 Suavimento exponencial = State Space Models (Exponential Smoothing)
 

#treino
ets_model = ets(y_train, allow.multiplicative.trend = TRUE)
summary(ets_model)

#avaliacao
ets_forecast = forecast(ets_model, h=4)
MAPE(ets_forecast$mean, y_test) *100  # 6.27  % erro

#plot FIT
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="ets Model - FIT (vermelho)", type='l')
lines(ets_forecast$fitted, col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)


#plot PREVISAO
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="ets Model", type='l')
lines(ets_forecast$mean, col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)

########################################################################################################################



#verificando sazonalidade
y  <- ts(as.vector(df_final[, c("GasolNaBomba")]))
y  <- ts(y, start = c(2018,07), end=c(2020,10), freq=12)  
log_y = log(y)

### Decompondo a Série Temporal

sazonal <- stl(log(y), s.window = "period")
# Plot Seasonal Decomposition
plot(sazonal, main = "Decomposição sazonal do log(Preco Medio Gasolina na Bomba)")

#qual atransformacao que precisamos fazer para a serie ser estacionaria 
ndiffs(y)
 y_d=diff(y)
plot(y_d)
summary(ur.kpss(diff(y)))
 
 
plot(y)
plot(d.y)

plot(diff(d.y))
d.y <- diff(y)

summary(ur.kpss(d.y))
 
 
###### 4.2  m ARIMA(0,0,2)
auto <- auto.arima(y_train)
summary(auto)
coeftest(auto)

#avaliacao
arima.forecast = forecast(auto,  4)  #  treino 1.68 
MAPE(arima.forecast$pred, y_test) *100  #  teste  1.39 
accuracy(auto)

#plot Fit
plot(y_train, col="blue", xlab="Ano", ylab="R$", main="Previsao do preco da gasolina com ARIMA - FIT", type='l')
lines(auto$fitted,  col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)
# plot previstos
 plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Previsao do preco da gasolina com ARIMA previsão - Forecast", type='l')
lines(arima.forecast$pred,    col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)
 
# hipotese nulo de que a autocorrelação é diferente de zero. 
Box.test(auto$residuals, type = "Ljung-Box") #p valor alto indica que altocorrelacao é igual a zero

# Checando normalidade ods residuos  
checkresiduals(auto)

############## 4.3  Modelo Arimax com regressores

auto2 <- auto.arima(y_train, xreg=x_train) #ARIMAX (0,0,0)
summary(auto2)
coeftest(auto2)
 

 
#avaliacao
arima2.forecast = forecast(auto2,  newxreg=auto2$xreg, n=4)#  treino 0.25%
accuracy(auto2)
MAPE(arima2.forecast$pred[1:4], y_test) *100  # 6.63 %
 
#plot Fit
plot(y_train, col="blue", xlab="Ano", ylab="R$", main="Previsao do preco da gasolina com ARIMAX - fit", type='l')
lines(auto2$fitted,  col="red", lwd=2)
grid(col='darkgrey',
     lwd=2)
#previsto no periodo teste
x <-ts(arima2.forecast$pred[1:3], start= c(2020,07), frequency = 12)

# plot previstos
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Previsao do preco da gasolina com ARIMA + regressores - Forecast", type='l')
lines(x , col="red", lwd=2)
grid(col='darkgrey',
     lwd=2) 
# hipotese nulo de que a autocorrelação é diferente de zero. 
Box.test(auto2$residuals, type = "Ljung-Box") #p valor alto indica que altocorrelacao é igual a zero

 
# Checando normalidade ods residuos  
checkresiduals(auto2)

 

################## 5. Redes Neurais 


  
#scaling train
maxs <- apply(training, 2, max) 
mins <- apply(training, 2, min)
scaled_train <- as.data.frame(scale(training, center = mins, scale = maxs - mins))   
    
#scaling test
scaled_test <- as.data.frame(scale(test, center = mins, scale = maxs - mins))   

set.seed(1)
 
n <- names(training)
 
    
 nn <- neuralnet(GasolNaBomba~Dolar+CtBarril + PCTribEst +Etanol+ IPCA+EstEmp + CDI+
                   IBC,
                data = scaled_train,
                hidden =  6,
                linear.output = TRUE,
                lifesign = 'full',
                rep=100)
 
 
 nn.bp <- neuralnet(GasolNaBomba~Dolar+CtBarril + PCTribEst +Etanol+ IPCA+EstEmp + CDI+
                   IBC,
                 data = scaled_train,
                 hidden =  6,
                 linear.output = TRUE,
                 algorithm="backprop",
                 learningrate=0.01,
                 rep=100)
  
 
 #rede
 plot(nn)
 plot(nn.bp)

#pesos 
 nn$result.matrix 
 
 # Predict
 pr.nn <- compute(nn,scaled_test ) 
 pr.nn.bp <- compute(nn.bp,scaled_test ) 
  
 #revertendo o scale
 result<-pr.nn$net.result*(max(training$GasolNaBomba) - min(training$GasolNaBomba) ) + min(training$GasolNaBomba) 
 result
 
 result2<-pr.nn.bp$net.result*(max(training$GasolNaBomba) - min(training$GasolNaBomba) ) + min(training$GasolNaBomba) 
 result2
 
 
 
 #avaliacao
nn.forecast =  MAPE(result, test[,"GasolNaBomba"]) * 100 #3.68 %
nn.forecast

nn.bp.forecast =  MAPE(result2, test[,"GasolNaBomba"]) * 100 # 2.66%
 
nn.bp.forecast

result_ts<- ts(result, start = c(2020,07), end=c(2020,10), freq=12)  

result2_ts<- ts(result2, start = c(2020,07), end=c(2020,10), freq=12)  

 
#plot previsao NN
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Previsao do preco da gasolina com RNN", type='l')
lines(result_ts,  col="red", lwd=2) 
grid(col='darkgrey',
     lwd=2) 

#plot previsao NN backprop
plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$",
     main="Previsao do preco da gasolina com RNN com back propagation", type='l')
lines(result2_ts,  col="red", lwd=2)  
grid(col='darkgrey',
     lwd=2)          


 