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

# Generar datos para validacion cruzada: dejar el ultimo a˜no
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
