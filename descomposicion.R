## DESCOMPOSICIÓN DE SERIES DE TIEMPO:
	# Se emplean las siguientes funciones que se encuentran ya instaladas:
		# decompose(): descompone la serie en la forma clásica
		# Aditiva y Multiplicativa.
	#	Ejemplo:
data(co2)
desc.aditiva<-decompose(co2)
desc.multipl<-decompose(co2,type='multiplicative')
plot(desc.aditiva)
plot(desc.multipl)
	#	Nótese que las gráficas difieren en su escala pero no en su forma.

		# stl(): Descompone la serie usando suavizamiento 'loess' mostrando
		# sus componentes tendencia, estacional e irregular
stl.co2<-stl(co2,12)
plot(stl.co2)
