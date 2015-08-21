# Instalación de paquetes (si no están instalados)
# Para revisar cuáles paquetes están instalados se emplea el comando library()

install.packages(c("xlsx","tseries","plyr","dplyr",
"forecast","hts","phtt","tseries","TSA","urca","fArma","pdR"))

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
library(HH) # Está incorporada
library(pdR)

#Lectura de Datos:
# Excel:
dolar=read.xlsx("seriedolar.xlsx",sheetName="mensual",
        colIndex=1:3)
# Texto plano
dolar<-read.table("dolarmensual.txt",header=T,sep="\t",dec=",")

# Visualizamos la base de datos para saber dónde empiezan las series:
head(dolar)	# Note que la serie hace referencia a la TRM promedio en el mes
		# y su valor al cierre a partir de enero de 1950 (787 observaciones)

# Seleccionamos una serie (p.ej. Fin de Mes)
y=ts(dolar$Fin.de.mes,start=c(1950,01),freq=12)

# Graficamos la serie:
plot(y)
	# ¿Qué se puede observar?
	# ¿Cuáles preguntas surgen a partir de estas observaciones?

## Analicemos gráficamente la serie y vemos qué pasa:

par(mfrow=c(1,3))
plot(y);acf(y);pacf(y)
par(mfrow=c(1,3))

	# ¿Qué vemos en la serie?
	# ¿Podemos describir algún comportamiento particular?
	# ¿Es necesario volverla estacionaria?
	# ¿Qué pasa si aplicamos la primera diferencia?
	# ¿Qué pasa si aplicamos la segunda diferencia?
	# ¿Qué pasa si aplicamos una diferencia de orden igual al número
	# de periodos? (12)

par(mfrow=c(3,3))
plot(diff(y));acf(diff(y));pacf(diff(y))
plot(diff(y,2));acf(diff(y,2));pacf(diff(y,2))
plot(diff(y,12));acf(diff(y,12));pacf(diff(y,12))
par(mfrow=c(1,3))

	# ¿Cuál serie seleccionamos y porqué?
	# Si seleccionamos la serie sin diferenciar ¿cómo se interpreta?
	# Si seleccionamos la serie con 1a diferencia ¿cómo se interpreta?
	# Si seleccionamos la serie con 2a diferencia ¿cómo se interpreta?
	# Si seleccionamos la serie con 12a diferencia ¿cómo se interpreta?

# ¿Y  si aplicamos logaritmo natural y luego la diferenciamos?

# Compararemos la serie sin diferenciar
# y la serie en logaritmo y primera diferencia
# para comparar su comportamiento.

y1=y
y2=diff(log(y))

par(mfrow=c(2,3))
plot(y1);acf(y1);pacf(y1)
plot(y2);acf(y2);pacf(y2)
par(mfrow=c(1,3))

# Existe un TEST de Estacionariedad (KPSS):
# el test KPSS (Kwiatkowski-Phillips-Schmidt-Shin) plantea como H0 la presencia de
# estacionariedad

# Veamos ejemplos para entender la prueba:
##KPSS test Estacionariedad ST

xp = rnorm(1000)  # Estacionaria
plot(ts(x))
kpss.test(x)

yp = cumsum(x)  # No Estacionaria
plot(ts(y))
kpss.test(y)

zp = 0.3*(1:1000)+rnorm(1000)  # Estacionaria con Tendencia
plot(ts(z))
kpss.test(z, null = "Trend")

# Probemos las series
kpss.test(y1);kpss.test(y2)
	# ¿Cuál de la series es estacionaria?
	# ¿Cuál serie es ARMA, cuál serie es ARIMA?

# ¿Qué dice la prueba de raíz unitaria?
summary(ur.df(y1)); summary(ur.df(y2))
	# ¿Justifica, entonces, el uso de integración (d) en alguna de las series?

# A los procesos ARMA también se les puede realizar pruebas de raíz unitaria
# la idea es que toda las raíces del proceso se encuentren fuera del círculo unitario
# esto implica que es posible invertir el proceso ARMA para realizar predicciones
# sobre el modelo. Ejemplo:

par(mfrow = c(1, 2), cex = 0.7)
coef = c(0.3, -0.3)
armaRoots(coef)
par(mfrow = c(1, 1), cex = 0.7)

# Probemos un modelo para y2 ARMA(0,1):
par(mfrow = c(1, 2), cex = 0.7)
armaRoots(arma(y2,order=c(0,1))$coef)
par(mfrow = c(1, 1), cex = 0.7)

# El análisis del ACF y PACF proponen un modelo ARMA(0,1) para la serie diferenciada
# en su logaritmo. Probemos el ajuste del modelo. Luego miramos el modelo ARIMA para
# la serie original.
## Método 1:
	# Estimación de los Parámetros:
	mody2.1 = armaFit(~ arma(0, 1), data = y2)
	print(mody2.1)
	# Análisis Diagnóstico del modelo:
	par(mfrow = c(2, 2), cex = 0.7)
	summary(mody2.1, which =  "all")
	par(mfrow = c(1, 1), cex = 0.7)

## Método 2:
	mody2.2 = arma(y2, order = c(0, 1))
	summary(mody2.2)
	# Análisis Diagnóstico del modelo:
	plot(mody2.2, which =  "all")

## Método 3:
	mody2.3 = arima(y2, order = c(0,0,1))
	summary(mody2.3)
	# Análisis Diagnóstico del modelo:
	tsdiag(mody2.3)

# Predicciones:
	# Usamos el comando predict() para realizar predicciones:
	predict(mody2.1,12)
	predict(mody2.2,12)
	predict(mody2.3,12)
		# ¿Qué pasa con los resultados?
		# ¿Cuál es preferible emplear y porqué?

	tsdiag(mody2.3)
	attributes(mody2.3)
	plot(mody2.3$residuals)
	acf(mody2.3$residuals)
	y.p=predict(mody2.3, n.ahead = 12,col=1:4)
	
	# Gráfica de las predicciones
	plot(y.p$pred)
	Yap=append(Y,y.p$pred); Yap
	plot.ts(Yap); abline(v=290, col="red")
	plot.ts(cbind(y2,y.p$pred))

		# ¿Qué significan esas predicciones?
		# ¿Son confiables?
		# ¿Qué habría que incorporar para interpretar adecuadamente las predicciones?

# Veamos, ahora, cómo tratamos el modelo para la serie original.
# Previamente, hemos observado un comportamiento no estacionario
# La ACF y el Test de Raíz Unitaria sugieren una valor d=1
# El test KPSS nos permitió comprobar que se trata de una serie no estacionaria

	# Veamos algunsa gráficas algo más descriptivas:

tsacfplots(y1)
acf.pacf.plot(y1)

	# ¿Qué se observa en estas gráficas?	
	# ¿Que tipo de combinaciones podemos probar?
	# ¿Podemos emplear d=0,1 y AR=1,2?
	# ¿Qué pasa con MA?

# Veamos si podemos describir algún orden para ARs MAs entre estaciones
res<-armasubsets(y=y1,nar=14,nma=14,y.name='y',ar.method='ols')
plot(res)

	# No se aprecia un orden específico claro
	# Se puede probar: AR=1,3,6 MA= 2,10
	# Entre estaciones: ARs=0 MAs=0,1,2

# ¿Es posible realizar alguna prueba de raíz para las estaciones?
# R tiene un procedimiento para ello.

hegytest<-HEGY.test(wts=y, itsd=c(1,0,c(1:3)),regvar=0, 
selectlags=list(mode="aic", Pmax=12))

hegytest$stats

	# Sólo se observa una raiz unitaria lo que sugiere que d=1 y D=0
	# Se probará con modelos D=0,1

## En resumen, se tienen los siguientes modelos para probar:
	# AR = 1,2
	# MA = 2,10
	# ARs=0
	# MAs=0,1,2
	# d=1 D=0,1
	# ¿Cuántos modelos se pueden probar?

# Revisemos más opciones del test de raíz unitaria:

lc.df1 <- ur.df(y=y1, lags=3, type='trend')
summary(lc.df1)
lc.df2 <- ur.df(y=y1, lags=3, type='none')
summary(lc.df2)
lc.df3 <- ur.df(y=y1, lags=3, type='drift')
summary(lc.df3)

# DF sugiere reíz unitaria con drift

# Modelemos:
m1<-arima(y1,order=c(1,1,2), seasonal = list(order = c(0,0,1 ), period = 12))
Box.test(residuals(m1), lag = 12, type = c("Ljung-Box"), fitdf = 0)
	# ¿Son los residuos Ruido Blanco?

m2<-arima(y1,order=c(2,1,2), seasonal = list(order = c(0,0,2 ), period = 12))
Box.test(residuals(m2), lag = 12, type = c("Ljung-Box"), fitdf = 0)
	# ¿Son los residuos Ruido Blanco?

m3<-arima(y1,order=c(2,1,2), seasonal = list(order = c(0,1,2 ), period = 12))
Box.test(residuals(m3), lag = 12, type = c("Ljung-Box"), fitdf = 0)

# Como el test DF sugiere que la serie tiene raíz unitaria tiene 'drift'
# es decir 'media diferente de cero', necesitamos emplear la función Arima

m4<-Arima(y1,order=c(2,1,2), seasonal = list(order = c(0,0,2 ), period = 12),include.drift=TRUE)
Box.test(residuals(m4), lag = 12, type = c("Ljung-Box"), fitdf = 0)

# Algunas predicciones con el modelo:
form4<-forecast(m4,level=c(95),h=12)
plot(form4)
