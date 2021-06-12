setwd('D:\\Trabalho_Bloco_A')
 

#Bibliotecas 
library(tseries)
library(tidyverse)
library(urca)
 
#importando o dataframe
df_final <-read.csv('df_final.csv')


#tratando numeros

df_final$IBC = as.numeric(gsub(",", ".", df_final$IBC ))
df_final$Estoque_Empregos = gsub("\\,","",df_final$Estoque_Empregos)
df_final$Estoque_Empregos = as.numeric(gsub("\\.","",df_final$Estoque_Empregos))



#filtrando df para BR
df<- df_final  %>% filter(Sigla_regiao == "BR")

#plotando variaveis
df_ts<-subset(df, select = cbind('mediaGasolinaBomba','IBC', 'PIM', 'CotacaoBrentReais', 'CotacaoDolar','MedianaIPCAMes','mediaCDI', 'Estoque_Empregos'))
df_ts<-ts(df_ts, start = c(2018,07), end=c(2020,07), freq=12)  
autoplot( df_ts, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Variáveis economicas e o Preço da Gasolina")



#verificando sazonalidade


# Create new fit on log scale series for seasonal decomposition
sazonal <- stl(log_y, s.window = "period")
# Plot Seasonal Decomposition
plot(sazonal, main = "Decomposição sazonal do log(Preco Medio Gasolina na Bomba)")



 
y  <- ts(as.vector(df[, c("mediaGasolinaBomba")]))
y  <- ts(y, start = c(2018,07), end=c(2020,07), freq=12)  


summary(ur.kpss(y))


#Stationarity Testing
#Autocorrelation Function (ACF)
#Ljung-Box test for independence.
#Augmented Dickey-Fuller (ADF) t-statistic test for unit root.
#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity.





# Dickey-Fuller test for variable
adf.test(y, alternative="stationary", k=0)
adf.test(y, alternative="explosive", k=0) 

# Augmented Dickey-Fuller test
adf.test(y, alternative="stationary")


summary(ur.df(y))



#separando em treino e Teste
training <- df[1:20,]  
test <- df[21:25,] 

#variavel endogena
y_train <- ts(as.vector(training[, c("mediaGasolinaBomba")]))
y_train <- ts(y_train, start = c(2018,07), end=c(2020,02), freq=12)  

y_test <- ts(as.vector(test[, c("mediaGasolinaBomba")]))
y_test <- ts(y_test, start = c(2020,03), end=c(2020,07), freq=12)  



#variavel exogena
x_train <- subset(training, select = cbind('IBC', 'PIM', 'CotacaoBrentReais', 'CotacaoDolar','MedianaIPCAMes','mediaCDI', 'Estoque_Empregos')) 
x_train <- ts(as.vector(x_train))
x_train <- ts(x_train, start = c(2018,07), end=c(2020,02), freq=12)  

x_test <- subset(test, select = cbind('IBC', 'PIM', 'CotacaoBrentReais', 'CotacaoDolar','MedianaIPCAMes','mediaCDI', 'Estoque_Empregos')) 
x_test <- ts(as.vector(x_test))
x_test <- ts(x_train,  start = c(2018,07), end=c(2020,02), freq=12) 


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


forecast_arima <- forecast(auto2, xreg =  x_test[,c('IBC', 'PIM', 'CotacaoBrentReais', 'CotacaoDolar','MedianaIPCAMes','mediaCDI', 'Estoque_Empregos')] )
plot(forecast_arima, xlab = "Data", ylab = "R$", main = "Previsao do preco da gasolina com ARIMA + regressores - teste")

MLmetrics::MAPE(forecast_arima$mean %>% as.numeric(), test[ , "mediaGasolinaBomba"]) 
MAE(forecast_arima$mean %>% as.numeric(),test[ , "mediaGasolinaBomba"])
RMSE(forecast_arima$mean %>% as.numeric(), test[ , "mediaGasolinaBomba"])

Rsquared <- 1 - (sum((y_test - forecast_arima$mean)^2)/sum((y_test - mean(test[ , "mediaGasolinaBomba"]))^2))
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
lmodel = lm(y_train~x_train) #Create the linear regression
summary(lmodel) #Review the results
 
y_heat<-predict(lmodel, newdata = x_test)
 
 plot(y_heat[1:5],y[21:25])
 
 MLmetrics::MAPE(y_heat[1:5]%>% as.numeric(), test[ , "mediaGasolinaBomba"]) 
 MAE(y_heat[1:5] %>% as.numeric(),test[ , "mediaGasolinaBomba"])
 RMSE(y_heat[1:5]as.numeric(), test[ , "mediaGasolinaBomba"])
 
 Rsquared <- 1 - (sum((y_test - y_heat[1:5]n)^2)/sum((y_test - mean(test[ , "mediaGasolinaBomba"]))^2))
 Rsquared 
 
######################## deep fucking learning

 
 
 library(keras)
 library(mlbench)
 library(dplyr)
 library(magrittr)
 library(neuralnet)

   
 n <- neuralnet(mediaGasolinaBomba~IBC+PIM+CotacaoBrentReais+CotacaoDolar+MedianaIPCAMes+mediaCDI+Estoque_Empregos,
                data = training,
                hidden = c(8,5),
                linear.output = F,
                lifesign = 'full',
                rep=1)
 
 #rede
 plot(n,col.hidden = 'darkgreen',     
      col.hidden.synapse = 'darkgreen',
      show.weights = F,
      information = F,
      fill = 'lightblue')

 training <- as.matrix(training)
 dimnames(data) <- NULL
 
 set.seed(123)
 ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
 training <- data[ind==1,1:13]
 test <- data[ind==2, 1:13]
 trainingtarget <- data[ind==1, 14]
 testtarget <- data[ind==2, 14]
 
 
#scaling
 
 m <- colMeans(x)
 s <- apply(training, 2, sd)
 training <- scale(training, center = m, scale = s)
 test <- scale(test, center = m, scale = s)
 
 str(trainingtarget)
 
 str(testtarget)
 
 
 #Model Creation
 model <- keras_model_sequential()
 model %>%
   layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
   layer_dense(units = 1)
 
 
 #Model Compilation
 model %>% compile(loss = 'mse',
                   optimizer = 'rmsprop', 
                   metrics = 'mae') 
 
 #Model Fitting
 mymodel <- model %>%          
   fit(training,trainingtarget,
       epochs = 100,
       batch_size = 32,
       validation_split = 0.2)
 
 #predict
 
 model %>% evaluate(test, testtarget)
 pred <- model %>% predict(test)
 mean((testtarget-pred)^2) 
 plot(testtarget, pred) 
 
 
  
  