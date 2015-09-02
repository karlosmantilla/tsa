# Crear las funciones para observar las tendencias:
t=seq(1:100)
t2=t^2
b0=1
b1=1
b2=1
par(mfrow=c(2,2))
# b1,b2>0
plot(t,b0+b1*t+b2*t2, type="l")
# b1,b2>0
plot(t,b0+(-b1)*t+(-b2)*t2, type="l")
# b1>,b2<0
plot(t,b0+b1*t+(-b2)*t2, type="l")
# b1<,b2>0
plot(t,b0+(-b1)*t+b2*t2, type="l")
par(mfrow=c(1,1))

#####################################

library(TSA)
library(tseries)
# Ejercicio Remesas:
# Leer los datos
remesas<-read.table("remesas.txt",header=T,sep="\t",dec=",")

# Convertir los datos en un objeto tipo ts
y = ts(remesas$remesas,frequency=12,start=c(2000,01))

# generar un vector de fechas, clase ’Date’
fechas = seq(as.Date("2000/1/1"), length.out = length(y), by = "months")

# grafica con fechas
ts.plot(y,main="Remesas")

# otros comandos para graficar con fechas con mas detalle: mes año
np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")
plot(fechas,y, main="Remesas", xaxt="n", panel.first = grid(),type="l",ylab="remesas.mes", lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = 0.2)

# Generar datos para validacion cruzada: dejar el ultimo año
T = length(y)
yi = y[1:(T-12)]
yf = y[(T-12+1):T]
# Ajustar 4 modelos: lineal, cuadratico, cubico, loglin
t = seq(1:(T-12))
t2 = t^2
t3 = t^3
lyi = log(yi)
# estimacion por minimos cuadrados ordinarios
mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)
mod.llin = lm(lyi~t) # auxiliar para el exponencial
summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)

# Modelo Exponencial lineal:
# paso 1) estimar el modelo auxiliar log linear
mod.llin = lm(lyi~t)
# paso 2) guardar los parametros del loglineal
b0.est = mod.llin$coefficient[1]
b1.est = mod.llin$coefficient[2]
# paso 3) guardar los datos en un data.frame
Ds = data.frame(yi,t)
# paso 4) usar la funcion nls
mod.exp = nls(yi~exp(beta0+beta1*t),
data=Ds,
start=list(beta0=b0.est, beta1=b1.est))
# paso 5) resultados
summary(mod.exp)

medidas = function(m,y,k){
# y = serie, m = modelo, k = numero parametros
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2)
mse = sse/(T-k)
R2 = 1-sse/ssr
Ra2 = 1-(T-1)*(1-R2)/(T-k)
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
M = c(Ra2, mse, aic, bic)
names(M) = c("R2ajus","MSE","logAIC","logBIC")
return(M)}

M.lin = medidas(mod.lin,yi,2)
M.cuad = medidas(mod.cuad,yi,3)
M.cub = medidas(mod.cub,yi,4)
M.exp = medidas(mod.exp,yi,2)
M = cbind(M.lin,M.cuad,M.cub,M.exp)
(M)


# Chequeo de las Hipótesis del Modelo de Regresión

r = mod.cuad$residuals
par(mfrow=c(2,2))
plot(t,r,type="o",ylab="residuo")
abline(h=0,lty=2)
plot(density(r),xlab="x",main= "",ylab="Densidad")
qqnorm(r)
qqline(r,col=2)
acf(r,ci.type="ma",60)
par(mfrow=c(1,1))

# Pronósticos sobre la tendencia:
tt = seq(164,175,1)
tt2 = tt^2
pr2 = predict(mod.cuad,data.frame(t=tt,t2=tt2))
plot(tt,yf,type="b")
lines(tt,pr2,col="red")

# ¿Son confiables los pronósticos en el corto plazo?

# Modelo para Componente Estacional

mes=season(y)
modeloest=lm(y~mes)
summary(modeloest)
