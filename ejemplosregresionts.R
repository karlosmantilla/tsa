library(TSA)

# Lineal
data(rwalk)
plot(rwalk,type="o",ylab="Caminata Aleatoria", xlab="Periodo")
modelo1=lm(rwalk~time(rwalk))
summary(modelo1)
abline(modelo1,col="red")

# Tendencia Cíclica o Estacional:

data(tempdub)
plot(tempdub,type="l",ylab="Temperatura",xlab="Periodo")
points(y=tempdub,x=time(tempdub),pch=as.vector(season(tempdub)))
mes=season(tempdub)
modelo2=lm(tempdub~mes-1)
summary(modelo2)
modelo3=lm(tempdub~mes)
summary(modelo3)

# Tendencia Coseno

armonico=harmonic(tempdub,1)
modelo4=lm(tempdub~armonico)
summary(modelo4)
plot(ts(fitted(modelo4),freq=12,start=c(1964,1)),ylab="Temperatura",type="l",
ylim=range(c(fitted(modelo4),tempdub)))
points(y=tempdub,x=time(tempdub),pch=as.vector(season(tempdub)),col="blue")
points(tempdub,type="o",col="red")

# Análisis de los Residuales

plot(y=rstudent(modelo3),x=as.vector(time(tempdub)),
xlab="Periodo",ylab="Residuos Estandarizados",type="o")
abline(h = 0, lty = 2, col = 8)

plot(y=rstudent(modelo3),x=as.vector(time(tempdub)),
xlab="Periodo",ylab="Residuos Estandarizados",type="l")
abline(h = 0, lty = 2, col = 8)
points(y=rstudent(modelo3),x=as.vector(time(tempdub)),
pch=as.vector(season(tempdub)))

plot(y=rstudent(modelo3),x=as.vector(fitted(modelo3)),
xlab="Valores Ajustados",ylab="Residuos Estandarizados",type="n")
abline(h = 0, lty = 2, col = 8)
points(y=rstudent(modelo3),x=as.vector(fitted(modelo3)),
pch=as.vector(season(tempdub)))

hist(rstudent(modelo3),xlab="Residuos Estandarizados")
qqnorm(rstudent(modelo3))
acf(rstudent(modelo3))

#### Serie Real (Inactivos Bucaramanga)
inactivos=read.table("inactivos.txt",header=T,sep="\t",dec=",")
seriei=ts(inactivos,start=c(2001,01),freq=12)
plot(seriei)
points(y=seriei,x=time(seriei),pch=as.vector(season(seriei)),col="blue")
mesi=season(seriei)
modeloi1=lm(seriei~mesi)
summary(modeloi1)

plot(y=rstudent(modeloi1),x=as.vector(fitted(modeloi1)),
xlab="Valores Ajustados",ylab="Residuos Estandarizados",type="n")
abline(h = 0, lty = 2, col = 8)
points(y=rstudent(modeloi1),x=as.vector(fitted(modeloi1)),
pch=as.vector(season(seriei)))

hist(rstudent(modeloi1))
qqnorm(rstudent(modeloi1))
acf(rstudent(modeloi1))

### Ejercicio Realizar el mismo procedimiento con las series de tasas de cambio.
