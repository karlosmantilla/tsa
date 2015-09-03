# Para acceder a la descripción de las bases de datos emplee los siguientes comandos:

library(TSA) # Si la librería no está instalada, proceda a instalarla.

data(nombredelabase)

?nombredelabase

# Donde 'nombredelabase' es la palabra que va antes de la extensión .dat

# La importación puede hacerla empleando:

base <- read.table("nombredelabase.dat",header=T,stringsAsFactors=F)

# La Expresión base corresponde al nombre que usted desee darle a la base de datos importada.

# Si la base de datos contiene más de una serie, realice el taller con cada una de las series de la base de datos.

# Para conocer el contenido de las mismas emplee el siguiente comando:

head(base)

# También puede importar las bases de datos empleando la expresión:

data(nombredelabase)

# Otras librerías:

library(TSA)
library(tseries)
library(forecast)
library(urca)
library(fUnitRoots)

# Comandos del Ejemplo de clase:

# EJEMPLO: Identificar el tipo de serie:

#Librerías útiles:
library(TSA)
library(tseries)
library(forecast)
library(urca)
library(fUnitRoots)

data(CREF)
?CREF
serie<-CREF

#Graficamos:
par(mfrow=c(2,3))
plot(serie)
acf(serie)
pacf(serie)
plot(diff(serie))
acf(diff(serie))
pacf(diff(serie))
par(mfrow=c(1,1))

# Prueba de Raíz Unitaria:
dfnone<-ur.df(serie,lags=12,type="none")
dftrend<-ur.df(serie,lags=12,type="trend")
dfdrift<-ur.df(serie,lags=12,type="drift")

# Resultados de la Prueba DF
summary(dfnone)
summary(dftrend)
summary(dfdrift)

# Proponer modelos:
m1<-arima(serie,order=c(0,1,0));m1$aic
m2<-arima(serie,order=c(1,1,0));m2$aic
m3<-arima(serie,order=c(1,1,1));m3$aic
m4<-arima(serie,order=c(0,1,1));m4$aic

# Serie tasa US/Yen
uno = read.table("yen_usd.txt",header=T,stringsAsFactors=F)
attach(uno)
y = ts(y,frequency=12, start=c(1973,01), end = c(1996,07))
ly = log(y)
## prueba df caso 3 = trend, con libreria urca
df.trend = ur.df(y = ly, lags = 0, type = "trend" )
summary(df.trend)


SOLUCIÓN PRIMERA PARTE DEL TALLER

# Cargamos las librerías
# Si no están instaladas vamos a \Paquetes\Instalar Paquetes(s)
# seleccionamos un CRAN y procedemos a instalar las librerías.

library(TSA)
library(tseries)
library(forecast)
library(urca)
library(fUnitRoots)

# Cargamos las bases de datos mediante el comando data()
data(airmiles)
data(beersales)
data(co2)
data(electricity)
data(gold)
data(oil.price)
data(units)

# Revisamos airmiles:
?airmiles # The revenue passenger miles flown
# by commercial airlines in the United States for each year from 1937 to 1960
par(mfrow=c(2,3))
plot(airmiles)
acf(airmiles)
pacf(airmiles)
plot(diff(airmiles))
acf(diff(airmiles))
pacf(diff(airmiles))
par(mfrow=c(1,1))
# Según las gráficas:
# La serie es estacionaria, es decir, apuntamos a un proceso ARMA(p,q)
# AR(p) porque la ACF decae rápidamente MA(q) porque PACF se corta abruptamente

# Realizamos la prueba de Raíz Unitaria:
summary(ur.df(airmiles,lags=12)) # No hay ur => ARMA

# Probamos con posibles combinaciones ARMA(1,1), ARMA(1,2); ARMA(2,1);ARMA(2,2)
airmod1<-arima(airmiles,order=c(1,0,1))
airmod2<-arima(airmiles,order=c(1,0,2))
airmod3<-arima(airmiles,order=c(2,0,1))
airmod4<-arima(airmiles,order=c(2,0,2))
AIC(airmod1,airmod2,airmod3,airmod4)
# Se trataría de un modelo ARMA(1,1). Sin embargo, es necesario tener en cuenta
# que existe un componente estacional (mes) que requiere modelarse.


# Revisamos beersales:
?beersales # Monthly beer sales in millions of barrels, 01/1975 - 12/1990
par(mfrow=c(2,3))
plot(beersales)
acf(beersales)
pacf(beersales)
plot(diff(beersales))
acf(diff(beersales))
pacf(diff(beersales))
par(mfrow=c(1,1))

# Realizamos la prueba de Raíz Unitaria:
summary(ur.df(beersales,lags=12)) # Hay ur => probamos ARIMA(p,1,q)
# Según pacf hay un proceso MA entre 3 y 5, se probarán varios modelos
# Es probable que el proceso AR se encuentre entre 5 y 7, se probarán varios modelos:
beermod1<-arima(beersales,order=c(5,1,1))
beermod2<-arima(beersales,order=c(6,1,1))
beermod3<-arima(beersales,order=c(7,1,1))
beermod4<-arima(beersales,order=c(5,1,2))
beermod5<-arima(beersales,order=c(6,1,2))
beermod6<-arima(beersales,order=c(7,1,2))
beermod7<-arima(beersales,order=c(5,1,3))
beermod8<-arima(beersales,order=c(6,1,3))
beermod9<-arima(beersales,order=c(7,1,3))
beermod10<-arima(beersales,order=c(5,1,4))
beermod11<-arima(beersales,order=c(6,1,4))
beermod12<-arima(beersales,order=c(5,1,5))

AIC(beermod1,beermod2,beermod3,
      beermod4,beermod5,beermod6,
      beermod7,beermod8,beermod9,
      beermod10,beermod12)
# Nos quedamos con el modelo 12: ARIMA(5,1,5). Sin embargo, es necesario tener en cuenta
# que existe un componente estacional (mes) que requiere modelarse.


# Revisamos co2
?co2 # Atmospheric concentrations of CO2 are expressed in parts per million (ppm) and
# reported in the preliminary 1997 SIO manometric mole fraction scale.
par(mfrow=c(2,3))
plot(co2)
acf(co2)
pacf(co2)
plot(diff(co2))
acf(diff(co2))
pacf(diff(co2))
par(mfrow=c(1,1))

# Parece una serie estacionaria ARMA(1,1)
co2mod1<-arima(co2,order=c(1,0,1));co2mod1$aic
co2mod2<-arima(co2,order=c(1,0,2));co2mod2$aic
co2mod3<-arima(co2,order=c(2,0,1));co2mod3$aic
summary(ur.df(co2,lags=12))
# Nos quedamos con el modelo co2mod2: ARMA(1,2); hay un elemento estacional
# que requiere modelarse.

# Revisamos electricity:
?electricity # Monthly U.S. electricity generation (in millions of
# kilowatt hours) of all types: coal, natural gas, nuclear, petroleum,
# and wind, 01/1973 - 12/2005
par(mfrow=c(2,3))
plot(electricity)
acf(electricity)
pacf(electricity)
plot(diff(electricity))
acf(diff(electricity))
pacf(diff(electricity))
par(mfrow=c(1,1))
# Parece una serie ARIMA
# Realizamos la prueba de raíz unitaria
summary(ur.df(electricity,lags=12))
# Usamos un modelo ARMA(1,2)
elecmod1<-arima(electricity,order = c(1,0,2))
elecmod2<-arima(electricity,order = c(1,0,1))
elecmod3<-arima(electricity,order = c(2,0,1))
AIC(elecmod1,elecmod2,elecmod3)
# Tomamos el modelo ARMA(1,1), pendiente la estacionalidad

# Serie gold
?gold # Daily morning gold prices in US dollars. 1 January 1985 - 31 March 1989.
par(mfrow=c(2,3))
plot(gold)
acf(gold)
pacf(gold)
plot(diff(gold))
acf(diff(gold))
pacf(diff(gold))
par(mfrow=c(1,1))
# No es posible realizar algunas estimaciones debido a los 'NA'
# Probamos algunos modelos
summary(ur.df(gold,lags=12))
# No hay mucha información para seleccionar el modelo
