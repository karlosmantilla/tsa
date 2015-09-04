# Librerías:

install.packages(c("xlsx","tseries","plyr","dplyr",
"forecast","hts","phtt","tseries","TSA","urca","fArma","pdR"))

install.packages(c("dplyr","HH"))

library(lattice)
library(xlsx)
library(plyr)
library(dplyr)
library(forecast)
library(hts)
library(phtt)
library(tseries)
library(TSA)
library(urca)
library(fArma)
library(HH)
library(pdR)

yen<-read.table("yen_usd.txt",header=T)
head(yen)
y<-ts(yen$y,start=c(1973,1),freq=12)
y
plot(y, main='yen', xlab='Años', ylab='cambio yen - dolar')

# Funciones de Autocorrelación:
par(mfrow=c(2,2))
acf(y)
pacf(y)
acf(diff(y))
pacf(diff(y))
par(mfrow=c(1,1))

# La parte estacional:
wd=as.vector(diff(diff(window(y),12)))

par(mfrow=c(1,2))
acf(wd,lag.max=36); pacf(wd,lag.max=24)
par(mfrow=c(1,1))

yd<-diff(y)

m1<-arima(yd,order=c(0,0,1))
m2<-arima(yd,order=c(0,0,2))
m3<-arima(yd,order=c(0,0,3))
m1$aic;m2$aic;m3$aic

plot(yd,lty=2)
lines(fitted(m1),col='red')

## Modelo Arima:
m1.arima<-arima(y,order=c(1,1,1))
m1.arima
tsdiag(m1.arima)
r1<-residuals(m1.arima)
Box.test(r1, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m2.arima<-arima(y,order=c(1,1,0))
m2.arima
tsdiag(m2.arima)
r2<-residuals(m2.arima)
Box.test(r2, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m3.arima<-arima(y,order=c(2,1,1))
m3.arima
tsdiag(m3.arima)
r3<-residuals(m3.arima)
Box.test(r3, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m4.arima<-arima(y,order=c(3,1,1))
m4.arima
tsdiag(m4.arima)
r4<-residuals(m4.arima)
Box.test(r3, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m1.arima$aic;m4.arima$aic

m5.arima<-arima(y,order=c(4,1,2))
m5.arima
tsdiag(m5.arima)
r5<-residuals(m5.arima)
Box.test(r5, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m6.arima<-arima(y,order=c(4,1,3))
m6.arima
tsdiag(m6.arima)
r6<-residuals(m6.arima)
Box.test(r6, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m6.arima$aic;m5.arima$aic

# El modelo 5 es un mejor modelo que el 1.

# Modelemos la parte estacional a ver si mejora el modelo.

m6<-arima(y,order=c(4,1,3), seasonal = list(order = c(1,0,0), period = 12))
m6
tsdiag(m6)
r6<-residuals(m6)
Box.test(r6, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m7<-arima(y,order=c(3,1,3), seasonal = list(order = c(1,0,0), period = 12))
m7
tsdiag(m7)
r7<-residuals(m7)
Box.test(r7, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m8<-arima(y,order=c(3,1,3), seasonal = list(order = c(1,1,1), period = 12))
m8
tsdiag(m8)
r8<-residuals(m8)
Box.test(r8, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m8$aic;m7$aic

m9<-arima(y,order=c(3,1,3), seasonal = list(order = c(2,0,2), period = 12))
m9
tsdiag(m9)
r9<-residuals(m9)
Box.test(r9, lag = 12, type = c("Ljung-Box"), fitdf = 0)

m8$aic;m9$aic

# El modelo a emplear es el 8
# Comparemos Gráficamente:

par(lab=c(25,25,2))
plot(y,lwd=2,main='Tasa de Cambio Dolar - Yen',ylab='YJP/USD',
xlab='Años', sub='Cantidad de YJP por 1 USD')
lines(fitted(m8),col='red',lwd=2,lty=2)
lines(fitted(m5.arima),col='blue',lwd=2,lty=2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topright', c('Real', 'ARIMA Estacional', 'ARIMA'),
lty=c(1,2,2), lwd=2, col = c('black', 'red', 'blue'))

# Predicciones:
# Usamos el modelo 8 y el arima 5
pred.m8<-predict(m8,12)
pred.m5<-predict(m5.arima,12)

# valores reales del Yen (Fuente: http://fxtop.com/)
yenreal<-c(107.865047,109.870911,112.357758,112.267102,113.911394,117.764660,
122.929094,122.669101,125.555351,119.119025,114.286681,115.235649)
yenser<-ts(yenreal,start=c(1996,8),freq=12)

par(lab=c(12,12,5))

plot(yenser,lwd=2,main='Tasa de Cambio Dolar - Yen',ylab='YJP/USD',
xlab='Años', sub='Cantidad de YJP por 1 USD',ylim=c(90,125),type='o')
points(pred.m8$pred,col='red',lwd=2)
lines(pred.m8$pred,col='red',lwd=2,lty=2)
points(pred.m5$pred,col='blue',lwd=2)
lines(pred.m5$pred,col='blue',lwd=2,lty=2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topleft', c('Real', 'ARIMA Estacional', 'ARIMA'),
lty=c(1,2,2), lwd=2, col = c('black', 'red', 'blue'))


