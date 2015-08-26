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
dolar<-read.table("dolarmes.txt",header=T,sep="\t",dec=",")


#GRAFICO SERIE

yrc=ts(dolar$Fin.de.mes[481:787],start=c(1990,01),freq=12)
yrc
lyrc=log(yrc)

par(mfrow=c(1,3))
plot(yrc,ylab='TRM',xlab='Año')
plot(lyrc,ylab='LogTRM',xlab='Año')
plot(yrc,ylab='TRM',xlab='Año')
points(yrc,x=time(yrc),pch=as.vector(season(yrc)))
par(mfrow=c(1,1))

# Sólo la serie original y los meses:

par(lab=c(25,25,2))
plot(yrc,ylab='TRM',xlab='Año')
points(yrc,x=time(yrc),pch=as.vector(season(yrc)))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

##############################################################################33
##MODELOS PREINTERVENCION
#CORRELOGRAMAS PREINTERVENCION
w=as.vector(window(yrc))
par(mfrow=c(2,2))
acf(w,lag.max=36); pacf(w,lag.max=24)

wd=as.vector(diff(diff(window(lyrc),12)))
acf(wd,lag.max=36); pacf(wd,lag.max=24)

# ESTIMACION MODELO PRE-INTERVENCION m1_pre
m1_pre=arima(yrc,order=c(2,1,2), seasonal = list(order = c(0,0,2 ), period = 12))
Box.test(residuals(m1_pre), lag = 12, type = c("Ljung-Box"), fitdf = 0)
m1_pre
##DIAGNOSTICO DEL MODELO m1_pre
tsdiag(m1_pre)
r1=residuals(m1_pre); qqnorm(r1); hist(r1)
Box.test(r1,15,type = c("Ljung-Box")); shapiro.test(r1)

##VERIFICACION CORRELOGRAMAS RESIDUOS
acf(as.vector(r1)); pacf(as.vector(r1))

#2-ESTIMACION MODELO PRE-INTERVENCION m2_pre
m2_pre=arima(yrc,order=c(4,1,2), seasonal = list(order = c(0,0,2 ), period = 12))
m2_pre
##DIAGNOSTICO DEL MODELO m2_pre
tsdiag(m2_pre)
r2 = residuals(m2_pre); qqnorm(r2); hist(r2)
acf(r2,10); pacf(r2,10)
Box.test(r2,10,type="L"); shapiro.test(r2)

##PREDICCIONES
y2.f = predict(m2_pre,12); y2.f$pred; y2.f
ampl = qnorm(0.975)*cbind(-y2.f$se,0,y2.f$se)
y2.ff = y2.f$pred+ampl
colnames(y2.ff)= c("2.5%","50%","97.5%"); y2.ff 
#GRAFICO DE PREDICCIONES
plot(y2.ff, plot.type="single",lty=c(1:3),ylim=c(2500,3500))


#####################################################################################
#MODELO INTERVENCION
            
# 1-Los OUTLIERS  se incorporan como VARIABLES DUMMY con xreg: 
# 2-El efecto intervención banda cambiaria se incorpora mediante una
# FUNCION TRANSFER con xtransf y transfer
# 
# xtransf: MATRIZ CUYAS COLUMNAS SON LAS VARIABLES INPUT
# transfer: LISTA (orden MA, orden AR) de la FUNCION TRANSFER.
# Se probará con c(1,0) pues se considerará el efecto inmediato de la política cambiaria.

#ESTIMACION MODELO INTERVENCION

## DEFINICION DE LA VARIABLE DUMMY INTERVENCION:

##### MODELO 1: INTERVENCION CON EL EFECTO 11 SEPTIEMBRE

IbandC=1*(seq(yrc)==9*12+9		## EFECTO BANDA CAMBIARIA (Septiembre 1999)
IChanC=1*(seq(yrc)==13*12+3)		## EFECTO 'FRENO'

dolar.m1=arimax(yrc,order=c(4,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(1,0),c(1,0)))
dolar.m1
m2_pre$aic;dolar.m1$aic

# Comparación Observaciones - Ajuste Pre-Intervención
plot(yrc,ylab='TRM',type="o")
points(fitted(m2_pre),col="blue",type="o")
legend('topleft', c("Real", "Pre-intervención"), lty=1, col = c('black', 'blue'))

# Comparación Observaciones - Intervención
plot(yrc,ylab='TRM')
points(fitted(dolar.m1),col="red",type="o")
legend('topleft', c("Real", "Intervención"), lty=1, col = c('black', 'red'))
par(mfrow=c(1,1))

tsdiag(dolar.m1)
par(mfrow=c(1,1))
                                      
## DEFINICION DE LAS VARIABLES DUMMY: (OUTLIERS)

##### MODELO 2: INCORPORACION DE LAS VARIABLES OUTLIERS

Jul2015=1*(seq(yrc)==307) ## UNA OBSERVACION OUTLIER
out=data.frame(Jul2015)

dolar.m2 = arimax(yrc,order=c(4,1,2),seasonal=list(order=c(0,0,1),period=12),
xtransf=data.frame(IbandC),transfer=list(c(0,0)), xreg=out, method='ML')
dolar.m2
dolar.m2$aic

pred2=fitted(dolar.m2)
plot(yrc,ylab='TRM',col='red')
points(pred2,col="blue")
lines(pred2,col="blue")

tsdiag(dolar.m2)
r3<-residuals(dolar.m2)
Box.test(r3,10,type="L"); shapiro.test(r3)
par(mfrow=c(1,2))
hist(r3)
qqnorm(r3)
qqline(r3)
par(mfrow=c(1,1))
