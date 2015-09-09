# PAQUETES:

install.packages(c("car","tseries","TSA","urca","fArma","vars"))

#LECTURA DE DATOS
datos<-read.table("dolarmoro.txt", header=TRUE, sep="\t", dec=",")
head(datos)
attach(datos)

# RESUMEN BÁSICO
summary(datos[,3:5])
var(datos[,3:7])
sd(USD_COP); sd(M_USA); sd(ORO)
dolar<-USD_COP;ofmon<-M_USA;oro<-ORO/10;mes<-factor(MES)
#GRÁFICAS#
par(mfrow=c(1,2))
plot(dolar, ofmon,pch=4); plot(ofmon, dolar,pch=4)
plot(datos[,3:5])
par(mfrow=c(1,1))
plot(mes,dolar)
plot(mes,oro)
plot(mes,ofmon)
# SERIE DE TIEMPO
series<-ts(data.frame(dolar,ofmon,oro),start=c(1994,1),freq=12)

ts.plot(series, lty=c(3:5),col=c(1:3),lwd=2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft", legend=c('Dolar','Oferta Monetaria','Oro'), lty=c(3:5),col=c(1:3),lwd=2)


###PRIMER MODELO CON OLS#
dolar.ols<-lm(dolar ~ ofmon + oro)
oro.ols<-lm(oro ~ ofmon + dolar)
summary(dolar.ols); summary(oro.ols)
res.dolar<-residuals(dolar.ols); res.oro<-residuals(oro.ols)
library(TSA)
plot(MES, res.dolar, type="o");abline(h=0, lty=2)
plot(MES, res.oro, type="o"); abline(h=0, lty=2)


##TEST PARA IDENTIFICACIÓN DE LOS RETARDOS#
library(car)
library(tseries)
par(mfrow=c(1,2))
acf(res.dolar)
acf(res.dolar, type="partial")
durbinWatsonTest(dolar.ols, max.lag=12)
par(mfrow=c(1,2))
acf(res.oro)
acf(res.oro, type="partial")
durbinWatsonTest(oro.ols, max.lag=5)


# MODELO VAR
y<-datos
library(vars)
yvar<-(y[,3:5])
adf1 <- summary(ur.df(yvar[, "USD_COP"], type = "trend", lags = 2))
adf1
adf4 <- summary(ur.df(diff(yvar1[, "USD_COP"]), type = "trend", lags = 2))
adf4
summary(adf1)
VARselect(yvar, lag.max = 8, type = "both")
adf2 <- summary(ur.df(yvar[, "ORO"], type = "trend", lags = 2))
adf2
adf3 <- summary(ur.df(diff(yvar2[, "ORO"]), type = "trend", lags = 2))
adf3
summary(adf2)
VARselect(yvar, lag.max = 8, type = "both")
p1ct <- VAR(yvar, p = 4, type = "const")
p1ct
summary(p1ct)
res3<-residuals(p1ct)
resdolar<-res3[,"USD_COP"]
resoro<-res3[,"ORO"]
par(mfrow=c(2,2))
acf(resdolar)
acf(resdolar, type="partial")
acf(resoro)
acf(resoro, type="partial")

# LOS RESIDUOS SON RUIDO BLANCO?
# REALICEMOS EL TEST:
Box.test(resdolar, lag = 12, type = c("Ljung-Box"), fitdf = 0)
Box.test(resoro, lag = 12, type = c("Ljung-Box"), fitdf = 0)

# CÓMO QUEDA EL MODELO:
p1ct$varresult
