setwd('D:\\Trabalho_Bloco_A')
 

#todo
#Analise de fatores 
# Método de Análise Fatorial
#recomenda-se a ACP, quando o objetivo é determinar o número mínimo de fatores que respondem pela máxima variância nos dados, 
#sendo os fatores chamados componentes principais (MALHOTRA, 2001).
 
#Bibliotecas 
library(tseries)
library(tidyverse)
library(urca)
 
#importando o dataframe
df_final <-read.csv('df_brasil.csv')
df_final=df_final[-c(1,6,7,9)]

 
 
#plotando variaveis
 df_ts<-ts(df_final, start = c(2018,07), end=c(2020,10), freq=12)  
autoplot( df_ts, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Variáveis economicas e o Preço da Gasolina")



#verificando sazonalidade
y  <- ts(as.vector(df_final[, c("GasolNaBomba")]))
y  <- ts(y, start = c(2018,07), end=c(2020,10), freq=12)  

log_y = log(y)
# Create new fit on log scale series for seasonal decomposition
sazonal <- stl(y, s.window = "period")
# Plot Seasonal Decomposition
plot(sazonal, main = "Decomposição sazonal do log(Preco Medio Gasolina na Bomba)")

 

summary(ur.kpss(y))

 
#Test is of type: mu with 2 lags. 
#Value of test-statistic is: 0.3599

#Critical value for a significance level of: 
#  10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

#This time, the test statistic is tiny, and well 
#within the range we would expect for stationary data. 
#we can conclude that the   data are stationary.
#podemos utilizar o modelo arima 
plot(y)
plot(d.y)

plot(diff(d.y))
d.y <- diff(y)

summary(ur.kpss(d.y))
#Stationarity Testing
#Autocorrelation Function (ACF)
#Ljung-Box test for independence.
#Augmented Dickey-Fuller (ADF) t-statistic test for unit root.
#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity.





# Dickey-Fuller test for variable
adf.test(d.y, alternative="stationary", k=0)
adf.test(d.y, alternative="explosive", k=0) 

# Augmented Dickey-Fuller test
adf.test(y, alternative="stationary")


summary(ur.df(d.y))



#separando em treino e Teste
training <- df_final[1:23,]  
test <- df_final[24:28,] 

#variavel endogena
y_train <- ts(as.vector(training[, c("GasolNaBomba")]))
y_train <- ts(y_train, start = c(2018,07), end=c(2020,05), freq=12)  

y_test <- ts(as.vector(test[, c("GasolNaBomba")]))
y_test <- ts(y_test, start = c(2020,06), end=c(2020,10), freq=12)  

 

#variavel exogena
x_train <- training['Dolar','CtBarril','Etanol','GasolProdr','PIM', 'IPCA','CDI', 'EstEmp','DistrRev','MRed','IBC']
x_train <- ts(as.vector(x_train))
x_train <- ts(x_train, start = c(2018,07), end=c(2020,05), freq=12)  

x_test <- subset(test, select = cbind('Dolar','CtBarril','Etanol','GasolProdr','PIM', 'IPCA','CDI', 'EstEmp','DistrRev','MRed','IBC')) 
x_test <- ts(as.vector(x_test))
x_test <- ts(x_train,  start = c(2020,06), end=c(2020,10), freq=12) 


# Função de autocorrelação (ACF) e a função de autocorrelação parcial (PACF):
Acf(y_train)  #exclui auto correlação do momento 0
Pacf(y_train) #exclui auto correlação do momento 0

#encontrando o arima automaticamente 

auto <- auto.arima(y_train,ic = "bic")
summary(auto)

#modelo com regressores
auto2 <- auto.arima(y_train, xreg=x_train)
summary(auto2)

#verificando coeficientes
coefficients(auto2)


# Check Accuracy
accuracy(auto)
accuracy(auto2)

 

#Modelo vs Dados reais
ggplot(auto2$fitted,aes(x,y))+geom_line(aes(color="Real"))+
  geom_line(data=y_train,aes(color="Modelado"))+
  xlab("Ano") + ylab("R$") +
  ggtitle("Modelo vs Historico - Fitted")+
  labs(color="Legend text")

# tabela com previstos
pred_arima <- forecast(auto,   20)
pred_arimax <- forecast(auto2 ,xreg=auto2$xreg )

 
# previsao 12
plot(pred_arima, xlab = "Data", ylab = "R$", main = "Previsao do preco da gasolina com ARIMA")
plot(pred_arimax, xlab = "Data", ylab = "R$", main = "Previsao do preco da gasolina com ARIMA + regressores")


#previsao no teste



 
# hipotese nulo de que a autocorrelação é diferente de zero. 
Box.test(auto$residuals, type = "Ljung-Box")
Box.test(auto2$residuals, type = "Ljung-Box") #p valor alto indica que altocrrelacao é igual a zero

# Checando normalidade ods residuos  
qqnorm(auto$residuals)
qqline(auto$residuals)

qqnorm(auto2$residuals)
qqline(auto2$residuals)


#plot analise residuos
checkresiduals(auto)
checkresiduals(auto2)




#Avaliacao do Modelo
#install.packages('MLmetrics')
library(MLmetrics)


forecast_arima <- forecast(auto2, xreg =  x_test[,cbind('Dolar','CtBarril','Etanol','GasolProdr','PIM', 'IPCA','CDI', 'EstEmp','DistrRev','MRed','IBC')] )
plot(forecast_arima, xlab = "Data", ylab = "R$", main = "Previsao do preco da gasolina com ARIMA + regressores - teste")

MLmetrics::MAPE(forecast_arima$mean %>% as.numeric(), test[ , "GasolNaBomba"]) 
MAE(forecast_arima$mean %>% as.numeric(),test[ , "GasolNaBomba"])
RMSE(forecast_arima$mean %>% as.numeric(), test[ , "GasolNaBomba"])

Rsquared <- 1 - (sum((y_test - forecast_arima$mean)^2)/sum((y_test - mean(test[ , "GasolNaBomba"]))^2))
Rsquared*100

plot(y[21:25] , forecast_arima$mean[1:5]  )


ggplot(y  ,aes(x,y))+geom_line(aes(color="Real")) +
 geom_line(data=forecast_arima   ,aes(color="Modelado"))+
  xlab("Ano") + ylab("R$") +
  ggtitle("Modelo vs Historico - Fitted")+
  labs(color="Legend text")

plot(y[21:25]) +
plot(forecast_arima$mean[1:5] )

 
fity <- auto.arima(y_train, xreg=x_train)
fitx <- auto.arima(x_train)
forecast(fity,h=10,xreg=forecast(fitx,h=10)$mean)



#modelo simples
lmodel = lm(df_final[,"GasolNaBomba"]~df_final[,"TribEst"] +df_final[,"GasolProdr"] ) #Create the linear regression
summary(lmodel) #Review the results

model = lm(log(df_final[,"GasolNaBomba"])~log(df_final[,"CtBarril"])) #Create the linear regression
summary(lmodel) #Review the results
 
y_heat<-predict(lmodel, newdata = x_test)
 
plot(y_heat[1:5],y[21:25])
 
 
 log(df_final[,"CtBarril"])

plot(round(df_final[,"Revenda"] / df_final[,"GasolNaBomba"] ,2) ,
round(df_final[,"Distr_trans"] / df_final[,"GasolNaBomba"] ,2)
)
 
regressao1 = lm(y_train~x_train)
summary(regressao1)

regressao=step(lm(df_final[,"GasolNaBomba"]~df_final[-c(13)]),direction = 'backfoward')
summary(regressao)



######## neural networks models

#neural network autoregression or NNAR model.

fit <- nnetar(df_ts[,"GasolNaBomba"],xreg = df_ts[,cbind('Dolar','CtBarril','Etanol','GasolProdr','PIM', 'IPCA','CDI', 'EstEmp','DistrRev','MRed','IBC')], lambda=0)
autoplot(forecast(fit,h=12, xreg=fit$xreg))



#SIMULACAO PARA DIVERSOS CENARIOS

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)



#roteiro


# Modelo naive simples (BASE LINE)

    naive = snaive(y_train, h=5)
    
    #avaliacao
    MAPE(naive$mean, y_test) * 100 #9,34% de erro
    
    #plot
    plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Seasonal Naive Forecast", type='l')
    lines(naive$mean, col="red", lwd=2)


#4) media movel (Base Line) <-


#### MEDIA MOVEL
    
library(smooth)
require(Mcomp)

#media simples 
m0 <- mean(y_train)
f_mean <- ts(rep(m0, each=5),start = c(2020,03), end=c(2020,07), freq=12)  
MAPE(f_mean, y_test) * 100 #9,44% de erro

plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Media Simples", type='l')
lines(f_mean, col="red", lwd=2)


 
#iniciando a variavel
m <- 0
m[0] <- MAPE(f_mean, y_test) * 100 # 
 
for(k in 1:12) { 
  print(cat("modelo ",k,"\n"))
  print(MAPE(forecast(sma(y_train ,order = k ,h=3, silent=FALSE),h = 5)$mean,y_test)*100) 
  m[k] <-MAPE(forecast(sma(y_train ,order = k ,h=3, silent=FALSE),h = 5)$mean,y_test)*100 
      }
 

#Suavimento exponencial = State Space Models (Exponential Smoothing)

    #treino
    ets_model = ets(y_train, allow.multiplicative.trend = TRUE)
    summary(ets_model)
   
     #avaliacao
    ets_forecast = forecast(ets_model, h=5)
    MAPE(ets_forecast$mean, y_test) *100  #10.08 % erro
    
    #plot
    plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Lts Model", type='l')
    lines(ets_forecast$mean, col="red", lwd=2)
    

#1) lm simples

#2) arima e arimax 
#3) neural network




######################## deep fucking learning 8.228417

 library(keras)
 library(mlbench)
 library(dplyr)
 library(magrittr)
 library(neuralnet)

  
    #scaling train
    
maxs <- apply(training, 2, max) 
mins <- apply(training, 2, min)
scaled_train <- as.data.frame(scale(training, center = mins, scale = maxs - mins))   
    
#scaling test
 
scaled_test <- as.data.frame(scale(test, center = mins, scale = maxs - mins))   




n <- names(training)
f <- as.formula(paste("GasolNaBomba ~", paste(n[!n %in% "GasolNaBomba"], collapse = " + "))) #equacao
    
 nn <- neuralnet(f,
                data = scaled_train,
                hidden =  5,
                linear.output = TRUE,
                lifesign = 'full',
                rep=1)
 
 #rede
 plot(nn)
 

#pesos 
 nn$result.matrix 
 
 # Predict
 pr.nn <- compute(nn,scaled_test) 
 
 
 #revertendo o scale
 result<-pr.nn$net.result*(max(training$GasolNaBomba) - min(training$GasolNaBomba) ) + min(training$GasolNaBomba) 
 
 MAPE(result, y_test) * 100 # 
 
 
 result_ts<- ts(result, start = c(2020,03), end=c(2020,07), freq=12) 
 #Faz o gráfico
 
 plot(df_ts[,"GasolNaBomba"], col="blue", xlab="Ano", ylab="R$", main="Rede Neural", type='l')
 lines( result_ts, col="red", lwd=2)
 
 
 