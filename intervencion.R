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
y=ts(dolar$Fin.de.mes,start=c(1950,01),freq=12)
y
ly=log(y)

par(mfrow=c(1,3))
plot(y,ylab='TRM',xlab='Año')
plot(ly,ylab='LogTRM',xlab='Año')
plot(y,ylab='TRM',xlab='Año')
points(y,x=time(y),pch=as.vector(season(y)))
par(mfrow=c(1,1))

# Sólo la serie original y los meses:

par(lab=c(25,25,2))
plot(y,ylab='TRM',xlab='Año')
points(y,x=time(y),pch=as.vector(season(y)))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

##############################################################################33
##MODELOS PREINTERVENCION
#CORRELOGRAMAS PREINTERVENCION
w=as.vector(window(y))
par(mfrow=c(2,2))
acf(w,lag.max=36); pacf(w,lag.max=24)

wd=as.vector(diff(diff(window(ly),12)))
acf(wd,lag.max=36); pacf(wd,lag.max=24)

# ESTIMACION MODELO PRE-INTERVENCION m1_pre
m1_pre=arima(y,order=c(2,1,2), seasonal = list(order = c(0,0,2 ), period = 12))
Box.test(residuals(m1_pre), lag = 12, type = c("Ljung-Box"), fitdf = 0)
m1_pre
##DIAGNOSTICO DEL MODELO m1_pre
tsdiag(m1_pre)
par(mfrow=c(1,2))
r1=residuals(m1_pre); qqnorm(r1); hist(r1)
Box.test(r1,15,type = c("Ljung-Box")); shapiro.test(r1)

##VERIFICACION CORRELOGRAMAS RESIDUOS
acf(as.vector(r1)); pacf(as.vector(r1))

#2-ESTIMACION MODELO PRE-INTERVENCION m2_pre
m2_pre=arima(y,order=c(3,1,2), seasonal = list(order = c(0,0,2), period = 12))
m2_pre
##DIAGNOSTICO DEL MODELO m2_pre
tsdiag(m2_pre)
par(mfrow=c(1,2))
r2 = residuals(m2_pre); qqnorm(r2); hist(r2)
acf(as.vector(r2)); pacf(as.vector(r2))
Box.test(r2,10,type="L"); shapiro.test(r2)

##PREDICCIONES
y2.f = predict(m2_pre,12); y2.f$pred; y2.f
ampl = qnorm(0.975)*cbind(-y2.f$se,0,y2.f$se)
y2.ff = y2.f$pred+ampl
colnames(y2.ff)= c("2.5%","50%","97.5%"); y2.ff 
#GRAFICO DE PREDICCIONES
par(mfrow=c(1,1))
plot(y2.ff, plot.type="single",lty=c(3,1,3),ylim=c(2500,3500))


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

##### MODELO 1: INTERVENCION CON EL EFECTO POLITICA CAMBIARIA

IbandC=1*(seq(y)==(1999-1950)*12+9) ## EFECTO BANDA CAMBIARIA (Septiembre 1999)
IChanC=1*(seq(y)==(1999-1950)*12+4) ## EFECTO 'FRENO'

dolar.m1=arimax(y,order=c(3,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(0,0),c(0,0)))
dolar.m1
m2_pre$aic;dolar.m1$aic

# Comparación Observaciones - Ajuste Pre-Intervención
par(mfrow=c(1,2))
plot(y,ylab='TRM',type="o")
points(fitted(m2_pre),col="blue",type="o")
legend('topleft', c("Real", "Pre-intervención"), lty=1, col = c('black', 'blue'))

# Comparación Observaciones - Intervención
plot(y,ylab='TRM',type="o")
points(fitted(dolar.m1),col="red",type="o")
legend('topleft', c("Real", "Intervención"), lty=1, col = c('black', 'red'))
par(mfrow=c(1,1))

tsdiag(dolar.m1)
par(mfrow=c(1,2))
r3 = residuals(dolar.m1); qqnorm(r3); hist(r3)
acf(as.vector(r3)); pacf(as.vector(r3))
Box.test(r3,10,type="L"); shapiro.test(r3)
                                      
## DEFINICION DE LAS VARIABLES DUMMY: (OUTLIERS)

##### MODELO 2: INCORPORACION DE LAS VARIABLES OUTLIERS
library(outliers)
outlier_tf = outlier(residuals(dolar.m1),logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
find_outlier

Abril2009=1*(seq(y)==712) ## UNA OBSERVACION OUTLIER
out=data.frame(Abril2009)

dolar.m2 = arimax(y,order=c(3,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(0,0),c(0,0)), xreg=out, method='ML')
dolar.m2
dolar.m2$aic
dolar.m1$aic

pred2=fitted(dolar.m2)
plot(y,ylab='TRM',col='red')
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


##NUEVOS PUNTOS OUTLIERS
outlier_tf = outlier(r3,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
find_outlier

Sep2008=1*(seq(y)==705)

out2=data.frame(Sep2008,Abril2009)

dolar.m3 = arimax(y,order=c(3,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(0,0),c(0,0)), xreg=out2, method='ML')
dolar.m3
dolar.m2$aic;dolar.m3$aic
pred3=fitted(dolar.m3)
plot(y,ylab='TRM',col="red"); points(pred3); lines(pred3,col="blue")
tsdiag(dolar.m3)
r4<-residuals(dolar.m3)
Box.test(r4,10,type="L")
shapiro.test(r4)

# MAS OUTLIERS?
Jul2015=1*(seq(y)==787)

out3=data.frame(Sep2008,Abril2009,Jul2015)
dolar.m4 = arimax(y,order=c(3,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(0,0),c(0,0)), xreg=out3, method='ML')
dolar.m4
dolar.m3$aic;dolar.m4$aic
r5<-residuals(dolar.m4)
Box.test(r5,12,type="L")
shapiro.test(r4)

outlier_tf = outlier(r4,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
find_outlier

Abr2015=1*(seq(y)==784)
out4=data.frame(out3,Abr2015)
dolar.m5 = arimax(y,order=c(3,1,2),seasonal=list(order=c(0,0,2),period=12),
xtransf=data.frame(IbandC,IChanC),transfer=list(c(0,0),c(0,0)), xreg=out4, method='ML')
dolar.m5
dolar.m5$aic;dolar.m3$aic
r6<-residuals(dolar.m5)
Box.test(r6,12,type="L")
tsdiag(dolar.m5)

outlier_tf = outlier(r6,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
find_outlier

pred3=fitted(dolar.m3)
plot(y,ylab='TRM',col='red')
points(pred3,col="blue")
lines(pred3,col="blue")

