G=read.table("areaconstruccionsantander.txt",header=T,sep="\t")
y<-G$Total.Area
y.ts<-ts(y,freq=12,start=c(1998,01))
y.ts
s.log=log(y.ts)
plot(s.log)

# Observemos varios aspectos de la serie original:
par(mfrow=c(1,2))
plot(y.ts,lty=2,main="Area Construida Santander");lines(decompose(y.ts)$trend,col="red");lines(decompose(y.ts)$trend+decompose(y.ts)$seasonal,col="blue")
plot(decompose(y.ts)$trend,col="red",main="Tendencia + C. Estacional");lines(decompose(y.ts)$trend+decompose(y.ts)$seasonal,col="blue",lty=2)
plot(decompose(y.ts)$trend,col="red", main="Tendencia");plot(decompose(y.ts)$trend+decompose(y.ts)$seasonal,col="blue",main="C. Estacional")

#Comparemos con el suavizamiento:
plot(s.log,lty=2);lines(decompose(s.log)$trend,col="red");lines(decompose(s.log)$trend+decompose(s.log)$seasonal,col="blue")
plot(decompose(s.log)$trend,col="red",main="Tendencia + C. Estacional");lines(decompose(s.log)$trend+decompose(s.log)$seasonal,col="blue",lty=2)
plot(decompose(s.log)$trend,col="red", main="Tendencia");plot(decompose(s.log)$trend+decompose(s.log)$seasonal,col="blue",main="C. Estacional")

# Comparemos Tendencias:
plot(decompose(y.ts)$trend,col="red", main="Serie Original")
plot(decompose(s.log)$trend,col="blue",main="Serie Suavizada")

# Comparemos C. Estacional
plot(decompose(y.ts)$seasonal,col="red",main="Serie Original")
plot(decompose(s.log)$seasonal,col="blue",main="Serie Siavizada")

# Veamos el modelo con tendencia lineal y estacionalidad:
library(forecast)
library(TSA)
t = seq(1,length(y.ts))
It = seasonaldummy(y.ts)
mod1 = lm(y ~ t + It)
summary(mod1)
?# Para LaTeX: library(xtable); print(xtable(mod1),digits=6)
r1 = mod1$residuals
yhat1 = mod1$fitted.values

par(mfrow=c(1,2))
acf(r1);acf(diff(s.log))

# Comparemos Serie vs Valores Ajustados:
yhat1.ts=ts(yhat1,freq=12,start=c(1998,01))
plot(y.ts,lty=2)
lines(yhat1.ts,col="red")

# ¿Qué tal es el diagnóstico?
# Diagnóstico
library(lmtest)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

