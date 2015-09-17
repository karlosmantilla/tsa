# Con base en la ENH se desea identificar cuáles son los factores que determinan
# la situación de empleo en un colombiano. Se va a emplear la información
# consolidada del DANE para junio de 2015 (última disponible en microdatos)
# se hará el cruce de información a partir de los distintos niveles de la
# Encuesta. Se realizará la comprobación mediante los modelos logit y probit
# y se comparará la calidad de información suministrada por cada uno de ellos.

# Lectura de datos:
ocu<-read.table('ocupados.txt', header=T, sep='\t', dec=',')
des<-read.table('desocupado.txt', header=T, sep='\t', dec=',')
per<-read.table('personas.txt', header=T, sep='\t', dec=',')
data1<-merge(ocu, des,by=c("idcomp","empleado","Dpto"), all = TRUE)
data2<-merge(data1,per,by=c("idcomp","Dpto"), all = TRUE)
ndata<-data2[,c("idcomp","empleado","Dpto","P6120")]
newdata <- na.omit(ndata[,c(2:4)])

# Reorganizamos variables:
empleo<-newdata$empleado
depto<-factor(newdata$Dpto)
aportes<-newdata$P6120

mydata<-data.frame(empleo,depto,aportes)

# Algunos datos básicos:
summary(mydata)
sapply(mydata, sd)
xtabs(~empleo + depto, data = mydata)

# calculamos el modelo Logit:

mylogit <- glm(empleo ~ ., data = mydata, family = "binomial")
summary(mylogit)

logLik(mylogit)

# calculamos el modelo probit:

myprobit <- glm(empleo ~ ., family = binomial(link = "probit"), data = mydata)
summary(myprobit)

logLik(myprobit)

# calculamos el modelo por regresión probabilística:

myprob <- lm(empleo ~ ., data = mydata)
summary(myprob)

logLik(myprobit)


# Comparamos los modelos:
logLik(myprobit)
logLik(mylogit)
logLik(myprob)

# Se observa que el mejor modelo es el logit.
# Como el modelo logit sigue una distribución Gamma se procede a comprobar
# si se cumple este supuesto mediante el test de wald usando la variable
# depto como agrupadora para verificar si su aporte es estadísticamente
# significativo. El test de wald se encuentra con la librería 'aod'
library(aod)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:13)

# el test sugiere que la variable dpto no aporta significativamente al modelo
por lo que se vuelve a calcular sin esta variable:

mylogit1 <- glm(empleo ~ aportes, data = mydata, family = "binomial")
summary(mylogit1)

# Calculamos el intervalo de confianza para los 'odds ratios':
exp(coef(mylogit1))
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))

# se observa que los coeficientes se encuentran dentro de los intervalos de
# confianza por lo que los coeficientes pueden ser adminitidos.

# veamos si ocurre lo mismo al consierar la variable depto:
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# se observa que, a pesar de no aportar significativamente al modelo
# los coeficientes de la variable 'depto' pueden ser adminitos dentros del
# ajuste.

# Procedemos a calcular nuevas observaciones:
newdata1 <- with(mydata, data.frame(aportes = mean(aportes),
	depto = depto))
newdata1$deptoP <- predict(mylogit, newdata = newdata1, type = "response")
head(newdata1)

newdata2 <- with(mydata, data.frame(aportes = rep(seq(from = 25000, to = 80000, length.out = 7310),
    4), depto = depto))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
    se = TRUE))
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata3)

library(ggplot2)

ggplot(newdata1, aes(x = newdata3$aportes, y = newdata3$PredictedProb)) + geom_ribbon(aes(ymin = newdata3$LL,
    ymax = newdata3$UL, fill = newdata3$depto), alpha = 0.2) + geom_line(aes(colour = newdata3$depto),
    size = 1)

# En la gráfica de las nuevas observaciones se puede apreciar cómo el comportamiento
# de la variable 'empleo' es similar en varios departamentos.
