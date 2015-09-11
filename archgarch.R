# Paquetes:

install.packages(c("tseries","plyr","dplyr",
"forecast","tseries","TSA","urca"))

library(plyr)
library(dplyr)
library(forecast)
library(tseries)
library(TSA)
library(urca)

# Se tiene el registro histórico del precio del petróleo medido semanalmente
# desde Enero 01 de 2010 hasta Agosto 31 de 2015 (296 datos)

# Lectura de datos:
petroleo<-read.table('petrosemanal.txt',header=T,sep='\t',dec=',')

# Convertimos a serie de tiempo:
y<-ts(petroleo$Cierre, start=c(2010,01),freq=52)

# Análisis Gráfico de Estacionariedad:
par(mfrow=c(1,2))
acf(y);pacf(y)
par(mfrow=c(1,1))
# La serie es No-Estacionaria.

# Prueba de Estacionariedad:

kpss.test(y) # ¿Es No-Estacionaria?

# Necesitamos volverla estacionaria, entonces, aplicamos logaritmo
# para suavizarla y primera diferencia para estacionarizarla:

ye<-diff(log(y))
# Graficamos:

plot(ye) # ¿Es volátil la serie?

par(mfrow=c(3,2))
acf(ye);pacf(ye)
acf(abs(ye));pacf(abs(ye))
acf(ye^2);pacf(ye^2)
par(mfrow=c(1,1))

# Revisemos la normalidad (recordemos el supuesto)
McLeod.Li.test(y=ye)
qqnorm(ye);qqline(ye)
shapiro.test(ye) # ¿Que nos dicen las pruebas sobre la normalidad?

# Veamos si podemos acercar a alguna estructura ARCH o GARCH:
eacf(ye^2)
eacf(abs(ye))

# Probemos, primero, una estructura ARMA(1,1)
mar<-arima(abs(ye),order=c(1,0,1))

# Estimemos el modelo GARCH o ARCH
# Vamos a probar algunos modelos y los comparamos:
m1<-garch(ye,order=c(0,1))
m2<-garch(ye,order=c(0,2))
m3<-garch(ye,order=c(1,1))
m4<-garch(ye,order=c(1,4))

AIC(m1,m2,m3,m4)

plot(residuals(m1),type='h',ylab='standardized residuals');abline(h=0)
qqnorm(residuals(m1))
qqline(residuals(m1))

acf(residuals(m1)^2,na.action=na.omit)
gBox(m1,method='squared')
gBox(m1,lags=20, plot=F,method='squared')$pvalue
acf(abs(residuals(m1)),na.action=na.omit)

plot((fitted(m1)[,1])^2,type='l',ylab='conditional variance',xlab='t')
