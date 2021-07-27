
# instalar paquete readxl
## La primera vez que li intenté instalar me lanzó una advertencia de que me faltaba rtools, ya lo instalé
#install.packages("readxl")
# cargar paquete readxl

library(readxl)

# buscar la ruta del archivo de excel
#file.choose()

# Copiar ruta de la consola y guardar en variable
ruta_excel <- "C:\\Users\\fcoyo\\Desktop\\Tesis\\BBDD Consolidada (version 1).xlsb.xlsx"
# como mirar las hojas de tu excel
excel_sheets(ruta_excel)


# PARA BORRAR OBJETOS USAMOS LA FUNCIÓN RM()
 #rm()
#####################################
# 2. importar excel con código de R #
#####################################

# importar caso ideal (La base empieza en la hoja 1 en la casilla 1) 
# En nuestro caso, en la hoja 1 la base tiene nombre muy largos para cada variable
# Mientras que en la segunda hoja las variables tienen nombres convenientes, por lo que elejimos la segunda opción de mas abajo

# Base_datos_no <- read_excel(ruta_excel)

# Caso en que la base no está en la primera hoja
Base_datos <- read_excel(ruta_excel,
                         sheet = "DATOS")

## Ya tenemos la base en R ahora hay que graficarla
# Instalaremos en paquete ggplot 

#install.packages("ggplot2")

library(ggplot2)

# Utilizamos ggplot para graficar con estilo

# ggplot(Base_datos)
# plot(Base_datos$tcambio_real)

# Probaré con un histograma

# hist(Base_datos)

#Puedo hacerlos para una variable
#Pruebo con grafico de barras
#primero genero tabla agrupada

# table(Base_datos$tcambio_real)
#luego procedo a graficar esa variable en barras
# barplot(table(Base_datos$tcambio_real))


# Reunion con los profes
# Explican como usar ggplot
ggplot(Base_datos, aes(x = periodo_tri, y = tcambio_real)) + 
  geom_line() +
  scale_x_continuous(name = "Nombre que yo elijo") +
  scale_y_continuous(name = "Falasia del Nirvana") + 
  ggtitle(label = "Grafico muy cool")

# Para añadir otra variable 
ggplot() + 
  geom_line(aes(x = Base_datos$periodo_tri, y = Base_datos$tcambio_real), color = "green") + 
  geom_line(aes(x = Base_datos$periodo_tri, y = Base_datos$tasa_IPC), color = "blue") +
  scale_x_continuous(name = "Periodo") +
  scale_y_continuous(name = "Valores") +
  ggtitle(label = "T.cbio Real vs IPC")

#Aún no soluciono el problema con el eje x (por algun motivo aqui en r, es el eje y)

# Probaré con el ejemplo de un r pubs que me dejó la profe

# library(lattice)
# library(ggplot2)
#install.packages("latticeExtra")
# latticeExtra must be loaded after ggplot2 to prevent masking of `layer`
#library(latticeExtra)
#library(RColorBrewer)
#lattice and latticeExtra configuration
#myTheme <- custom.theme.2(
  #pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')),
  #symbol=brewer.pal(n=8, name="Dark2"))
#myTheme$strip.background$col = myTheme$strip.shingle$col =
 # myTheme$strip.border$col = 'transparent'

#myArgs <- list(
 # as.table=TRUE, between=list(x=0.5, y=0.2),
  #xscale.components = function(...)
   # modifyList(xscale.components.default(...), list(top=FALSE)),
  #yscale.components = function(...)
   # modifyList(yscale.components.default(...), list(right=FALSE)))

#lattice.options(default.theme=myTheme, default.args=modifyList(
 # lattice.options()$default.args, myArgs))
#install.packages("zoo")
#library(zoo)

#Intentaré seguir el ejemplo a ver si aprendo a graficar con mi base de datos 

# download.file(
#  url = "https://rstudio-pubs-static.s3.amazonaws.com/aranjuez.RData", 
#  destfile = "Aranjuez"
# )
# Basededatis <- data.frame(Base_datos)
# xyplot(Basededatis, layout = c(1, ncol(Basededatis)))
# autoplot(Base_datos) + facet_free()   

#Los archivos que se usan en la publicacion ya no estan disponibles

##Estadística descriptiva
summary.data.frame(Base_datos)
Est.desc <- summary.data.frame(Base_datos)

Est.desc

## Le diré a r que estos datos son una serie de tiempo
#Para eso debo usar las siguientes librerias 

# install.packages("tidyverse")
# install.packages("stringi", type = "win.binary")

library(tidyverse)
library(lubridate)

#Base_datos.ts = ts(Base_datos, start = 1985, frequency = 4)
#Base_datos.ts

#plot(Base_datos.ts)


## Intento usar ggplot para graficar Base_datos.ts pero me dice que data debe ser un dataframe

# ggplot(Base_datos.ts, aes(x = tcambio_real, y = tasa_IPC )) + 
  #geom_line() +
  #scale_x_continuous(name = "Nombre que yo elijo") +
  #scale_y_continuous(name = "Falasia del Nirvana") + 
  #ggtitle(label = "Grafico muy cool")



#Agrego otra hoja a la base de datos Excel, en la que no hay columna 
#para los periodos, y genero la base a partir de ella, ya que al especificar
#que se trata de una serie de tiempo, esta se genera sola

Base_datos <- read_excel(ruta_excel,
                         sheet = "DATOS.TS")

Base_datos.ts = ts(Base_datos, start = 1985, frequency = 4)
Base_datos.ts
plot(Base_datos.ts)

#Usaré solo una variable 

# tcambio_real = data.frame(Base_datos[,1])
# precio_cobre = data.frame(Base_datos[,8])

#Esta es la forma correcta para series de tiempo
tcambio_real = ts(Base_datos.ts[,1], start=1985, freq=4)
tcambio_real

#Grafico

#plot(tcambio_real.ts)
plot(tcambio_real, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile")

#Ahora el precio del cobre
precio_cobre = ts(Base_datos.ts[,8], start=1985, freq=4)
precio_cobre

#plot(precio_cobre.ts)
plot(precio_cobre, ylab="Precio", xlab="Trimestres", main="Precio del cobre en Chile")

#Primero descargaré y activaré todas las librerias de series de tiempo, ya tengo tidyverse y lubridate
#install.packages("car")
#install.packages("urca")
#install.packages("tseries")
#install.packages("astsa")
#install.packages("forecast")
#install.packages("foreign")
#install.packages("timsac")
#install.packages("vars")
#install.packages("mFilter")
#install.packages("dynlm")
#install.packages("nlme")


library(car)
library(urca)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)

#Probaré Estacionariedad del precio del cobre(Por el grafico que hicimos ya se aprecia que no lo es)

# class(Base_datos)
# class(Base_datos.ts)

#Primero graficaré su estacionaariedad
seasonplot(precio_cobre, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - Precio del cobre")

#No se entiende mucho porque se grafica la estacionariedad dentro de cada año 

#Observaré la funcuón de autocorrelación a ver si se ve estacionaria
acf(precio_cobre)
#Se aprecia que va disminuyendo lentamente, así se ve una serie no estacionaria

######
#Para convertila a estaconiaria usaremos diferencias

#Hay un comando para saber cuantas diferencias requiere la serie para ser estacionaria

ndiffs(precio_cobre)

# Se genera un 1, indicando que hace falta la primera diferencia para logarlo.

precio_cobre_dif = diff(precio_cobre)
plot(precio_cobre_dif)

#La media es cercana a 0, ahora si se ve estacionaria (no es perfecta, pero aceptable)

ndiffs(precio_cobre_dif)

#Compruebo que ya no necesito diferencias, se genera un 0, ya es estacionaria 


# Analisis visual de las graficas de autocorrelacion

par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)

#Le estoy pidiendo que me muestre 1 solo cuadro con 4 gráficas, mfrow es para que me
# muestre 2 columnas y 2 renglones de mi grafica
# Quiero que me muestre primero mi serie original, luego la autocorralacion
# de esa serie no estacionaia, luego que me grefique la serie ya diferenciada y finalmente 
# su respectiva autocorrelacion, para observar las diferenias entre una serie estacionaria y
# una no estacionaria

plot(precio_cobre, ylab="Precio del cobre", xlab="Tiempo")
acf(precio_cobre, main="Serie no Estacionaria")
plot(precio_cobre_dif, ylab="Precio del cobre", xlab="Tiempo")
acf(precio_cobre_dif, main="Serie Estacionaria")

#Creo diferencias para tcambio real

ndiffs(tcambio_real)
tcambio_real_dif <- diff(tcambio_real)

plot(tcambio_real, ylab="tipo de cambio real", xlab="Tiempo")
acf(tcambio_real, main="Serie no Estacionaria")
plot(tcambio_real_dif, ylab="tipo de cambio real", xlab="Tiempo")
acf(tcambio_real_dif, main="Serie Estacionaria")

## Haré un arima 

#Me faltaba una librería

#install.packages("quantmod")
library(quantmod)

#Prueba de Dickey Fuller y Prueba de Ruido Blanco 

adf.test(precio_cobre, alternative = "stationary")

#da un valor p de 0,5098 > 0,05 (No es estacionaria)

adf.test(precio_cobre_dif, alternative = "stationary")

#ahora si da un valor p de 0,01 (< 0,05)
#Por ende es estacionaria

#veamos las funciones de autocorrelacion y autocorrelación Parcial

par(mfrow=c(2,1), mar=c(4,4,4,1) + .1)

acf(precio_cobre_dif, main="Función de Auto Correlación")
pacf(precio_cobre_dif, main="Función de Auto Correlación Parcial")


#La función de autocorrelación nos dice el número de medias moviles
#La función de autocorrelación parcial nos dice el número de autoregresivos,

#Para que el rezago coincida con las frecuencias, le damos la sig instrucción

acf(ts(precio_cobre_dif, frequency=1))
pacf(ts(precio_cobre_dif, frequency=1))

#No logro poner titulos}

#Para hacer el modelo arima se usa la serie original, no la diferenciada, se hace 
#un arima 0,1,2 (2 autoregresivos, 1 diferencia, 1 medias moviles)
# REVISAR, NO SE SI CONSIDERAR MÁS REZAGOS

modeloarimacobre = arima(precio_cobre, order=c(2,1,1))
modeloarimacobre

#Me entrega primero el coeficiente para el autoregresivo (ar1) y los coeficientes
#para las medias movieles (ma1 y ma2), abajito están los errores estándar para c/u

#Puedo graficarlo y hacer un diagnóstico

tsdiag(modeloarimacobre)

# Arriba están los errores estandarizados (que se deberían parecer al ruido blanco)
# Abajo están los valores p para el estadístico Ljung-Box (es para ver si hay ruido blanco)
#la linea azul es el valor p de 0,05, la prueba nos dice que son > a 0,05 
#esto quiere decir que hay ruido blanco y nuestro modelo se ajusta bien
# Tambien lo probaremos a traves de este estadistico, y se hace con el comando:

Box.test(residuals(modeloarimacobre), type = "Ljung-Box")

#Obtenemos un valor p de 0,77 > 0,05, por lo que hay ruido blanco
# Que el modelo se ajusta bien, significa: que el error tiene media = 0, 
#varianza constante, y no está serialmente correlacionada.

#Ahora veremos el error
error_precio_cobre = residuals(modeloarimacobre)

par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)

plot(error_precio_cobre)

#Haremos un pronostico de 4 años, solo por diversión 

pronostico_precio_cobre <- forecast::forecast(modeloarimacobre, h = 16)
pronostico_precio_cobre

#Entrega el pronostico con limite inferior y superior a 80 y 95% de confianza
#Por ejemplo, dice que en el primer trimestre del 2021 el precio será de 3,24
#aunque sabemos con un 95% de confianza que estará entre 2,74 y 3,73.

#Lo graficamos, para ver que bonito quedó
plot(pronostico_precio_cobre)

### Haré pruebas de cointegración

#Generaré una serie de tiempo con las variables precio del cobre y tipo de cambio real
cobreytcambio = cbind(precio_cobre, tcambio_real)
plot(cbind(precio_cobre, tcambio_real), main="Tendencia")

modelocobreytcambio=lm(precio_cobre ~ tcambio_real)
modelocobreytcambio
summary(modelocobreytcambio)
residuales_cobreytcambio = modelocobreytcambio$residuals
summary(residuales_cobreytcambio)

#En las pruebas de cointegrción se revisa que los errores son estacionarios o no

residualPlot(modelocobreytcambio)

#A simple vista se ve que los errores no son estacionarios

adf.test(residuales_cobreytcambio)
#p-value = 0,54 > 0,05 
y=ur.df(residuales_cobreytcambio, type = "trend",selectlags = "AIC")
summary(y)
#Me entrego los valores al 1, al 5, y al 10 %
#valor del test stadistico = - 2.6788, en valor absoluto 2,6788, es menor que 
# 3.13, por lo que estaría callendo en la zona de no rechazar H0, eso significa
#que existe raiz unitaria
y@teststat
y@cval
#Si el valor de t es menor en valor absoluto al critico del
#los residuales son No estacionarios (tienen raiz unitria)

##Tambien existe la prueba de Phillips y Ouliaris oara cointegracion
#No importa cual variable va al lado izquierso de la ecuación

prueba.P0=ca.po(cobreytcambio, type="Pz")

summary(prueba.P0)
# cae en la zona de no rechazar H0, lo que quiere decir que los residuales tienen
#raiz unitaria, y no están cointegradas

prueba.P02=ca.po(cobreytcambio, type="Pu")
summary(prueba.P02)

#También da un t statics menor al 10%, no estan cointegradas


####Recuerda que si los residuales no son estacionarios entonces las varriables no 
#están cointegradas

## Intentaremos con el modelo VAR

library(xts)

#Creo logaritmos a ver si mejora el comportamiento de las variables

logtcambio_real <- log(tcambio_real)
#ltcambio_real
logprecio_cobre <- log(precio_cobre)
#lprecio_cobre

#ts.plot(ltcambio_real)
#ts.plot(lprecio_cobre)

#par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)

#ts.plot(ltcambio_real, ylab="Precio", xlab="Tiempo")
#ts.plot(tcambio_real.ts, main="Tipo de cambio con Log y sin Log")
#ts.plot(lprecio_cobre, ylab="Precio", xlab="Tiempo")
#ts.plot(precio_cobre.ts, main="Precio del cobre con Log y sin Log")


#ndiffs(lprecio_cobre)

logtcambio_real_dif <- diff(logtcambio_real)
logprecio_cobre_dif <- diff(logprecio_cobre)

#plot(lprecio_cobre, ylab="Precio", xlab="Tiempo")
#acf(lprecio_cobre, main="Serie con Log no Estacionaria")
#plot(lprecio_cobre_dif, ylab="Precio", xlab="Tiempo")
#acf(lprecio_cobre_dif, main="Serie con Log Estacionaria")


par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)

#Para graficar
ts.plot(precio_cobre_dif, tcambio_real_dif, col=c( "blue", "red"))
ts.plot(logprecio_cobre_dif, logtcambio_real_dif, col=c( "blue", "red"))
# Se ve bien bonito

##Haremos el test de causalidad granger para determinar el orden causal

grangertest(logprecio_cobre_dif~logtcambio_real_dif, order = 1)

# H0: El tipo de cambio real no causa en el sentido de granger al precio del cobre > 0,05
# H1: El tipo de cambio real si causa en el sentido de grander al precio del cobre < 0,05

#A prueba y error 
# Con la prueba de primer orden nos da 0.005668, < 0,05, por ende (al usar las variables sin log da 0.007238)
# se rechaza la hipotesis nula, y por ende, se acepta que el tipo de cambio real
# causa al precio del cobre

grangertest(logtcambio_real_dif~logprecio_cobre_dif, order = 1)

# H0: El precio del cobre no causa en el sentido de granger al tipo de cambio real > 0,05
# H1: El precio del cobre si causa en el sentido de grander al tipo de cambio real < 0,05

# En este sentido nos da 0.03073 < 0,05 (al usar las variables sin log da 0.01681), 
#por ende tambien se rechaza la hipotesis nula, lo que
#quiere decir que el precio del cobre si causa al tipo de cambio real

###### Podría haberse dado el caso en que en alguna o en ambas direcciones no hubieramos
#encontrado causalidad, de ser ese el caso (y suponiendo que existen motivos para creer que 
# deberia existir la relación) tendriamos que ir probando con otros órdenes, 
#(Desde el 1 al 12) hasta ver cual satisface la relación, 

# Aunque ya encontramos causalidad en ambas direcciones, probamos con 2 rezagos

# grangertest(lprecio_cobre_dif~ltcambio_real_dif, order = 2)

#Obtenemos 0,06 > 0,05, por lo que se aceptaria
#la hipotesis nula, de que no hay relacion de causalidad en esa dirección.

## Creamos un nuevo objeto para VAR con las variables ya estacionarias

logtcambio_real_dif <- ts(logtcambio_real_dif, start = 1985, freq = 4)
logprecio_cobre_dif <- ts(logprecio_cobre_dif, start = 1985, freq = 4)

#Como era para poner etiquetas en variables, no queremos hacer nombres complicados
#de variables, pero tampoco quiero olvidar que es ejvar
# ejvar = var de ejemplo, aqui solo estan el tipo de cambio y el opprecio del cobre

ejvar <- cbind(logtcambio_real_dif, logprecio_cobre_dif)
ejvar

##PROCESO VAR

VARselect(ejvar, lag.max = 12)

##Obtengo que 2 de los criterios me dicen que es de orden 2, y otro 2
#me dicen que es de orden 1 ¿Por cuales nos guiamos? probamos con orden 1 (p=1)

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      1      1      2 

var1 <- VAR(ejvar, p=1)
var1

# Nos entrega los coeficientes de las 2 ecuaciones de regresión que tenemos 
#(y la constante), (1 rezago), para las 2 ecuaciones (la del tipo de cambio, y la del precio del cobre)

#Prueba de especificación del modelo VAR

summary(var1)

#Nos da las raíces de las características polinomiales, en lo que debemos fijarnos para 
#ver si se satisface la condición de estabilidad es los n°s que nos da arriba
#(bajo Roots of the characteristic polynomial:), sean menores a 1: En este
#caso son 0.2672 0.2672, y si son < a 1, por lo que si se satisface
#la condición de estabilidad (esto quiere decir que estamos usando el n° correcto de 
# rezagos, y que el orden de nuestro modelo VAR es 1)

#(Es un modelo de orden 1 donde se cumple la condición de estabilidad)
par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)

plot(var1)


#Se generan graficas separadas para cada variable

###Haremos la prueba de autocorrelación serial en los residuales

serialprecioycambio <- serial.test(var1, lags.pt = 1, type = "PT.asymptotic")
serialprecioycambio$serial

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05

#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#osea que sí hay presencia de correlación serial



#################Procedemos a hacer la prueba de normaliad de los residuales

normalidad=normality.test(var1)
normalidad$jb.mul

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

#(La kurtosis si me da < a 0,05, pero el sesgo me da 0,1072)
## Al parecer los errores no tieen una distribucion normal

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch1 <- arch.test(var1, lags.multi = 1)
#Me entreó algunos mensajes de warning
arch1$arch.mul
#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

#Se concluye que la varianza es contante, no podemos rechazar hipotesis nula (pvalue = 0,4509)

####Modelo impulso respuesta
# 1° Veremos el impulso respuesta del precio del cobre, ante una innovación en el tipo de cambio

var1_irf_pcob_tcam=irf(var1, response ="logprecio_cobre_dif", n.ahead=8, boot=TRUE)
var1_irf_pcob_tcam

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso del tipo de cambio real
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

plot(var1_irf_pcob_tcam)

##Tambien lo hacemos para el tipo de cambio, efectos ante cambios en el precio del cobre

var1_irf_tcam_pcob=irf(var1, response ="logtcambio_real_dif", n.ahead=8, boot=TRUE)
var1_irf_tcam_pcob

plot(var1_irf_tcam_pcob)

#Descompocición de la varianza ante una innovación en el precio del cobre
        #(le estamos pidiendo 50 observaciones hacia adelante como pronostico)
var1_DESVAR_pcob=fevd(var1, n.ahead=50)$logprecio_cobre_dif
var1_DESVAR_pcob

#Descompocición de la varianza ante una innovación en el tipo de cambio real
var1_DESVAR_tcam=fevd(var1, n.ahead=50)$logtcambio_real_dif
var1_DESVAR_tcam

### CAMBIOS ESTRUCTURALES
#Generamos un modelo de regresion respecto a una constante  (le pusimos 1)
#Prueba de Chow 

modelo=Fstats(precio_cobre~1, from = .01)

#Prueba para cambios estructurales
sctest(modelo)

#H0: No hay cambio estructural en la serie (pvalue>0.05)
#H1: Hay cambio estructural en la serie (pvalue<0.05)

#Entrega un valor p muy pequeño, determinamos que se rechaza H0, hay cambio estructural

### La prueba estadistica de Chow nos dice que sí hay cambios estructurales
##Lo que sigue es saber en que fecha estuvieron esos cambios y graficar

strucchange::breakpoints(precio_cobre~1)

#Dice que la podemos partir en 3 partes, los quiebres serian en el 4° trimestre
#del 2005, y el 4° trimestre del 2014
## Investigué, el cobre cerró el 2005 con un precio histórico debido a la escacez
#de reservas provocada por un aumento de demanda China
# El 2014 el precio alcanza su nivel más bajo, superando la crisis del 2008
#Debido a la desaceleración de la economía del gigante asiatico

BP=strucchange::breakpoints(precio_cobre~1)

summary(BP)

#Dice que hay 5 fuertes cambios estructurales, pero lo dividiermos por 3 como 
plot(BP)
plot(precio_cobre)
lines(BP)

## Revisamos los intervalos e confianza para probar los cambios estructurales

Interconf=confint(BP)
Interconf
lines(Interconf)
