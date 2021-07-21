
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

library(lattice)
library(ggplot2)
#install.packages("latticeExtra")
# latticeExtra must be loaded after ggplot2 to prevent masking of `layer`
library(latticeExtra)
library(RColorBrewer)
# lattice and latticeExtra configuration
myTheme <- custom.theme.2(
  pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')),
  symbol=brewer.pal(n=8, name="Dark2"))
myTheme$strip.background$col = myTheme$strip.shingle$col =
  myTheme$strip.border$col = 'transparent'

myArgs <- list(
  as.table=TRUE, between=list(x=0.5, y=0.2),
  xscale.components = function(...)
    modifyList(xscale.components.default(...), list(top=FALSE)),
  yscale.components = function(...)
    modifyList(yscale.components.default(...), list(right=FALSE)))

lattice.options(default.theme=myTheme, default.args=modifyList(
  lattice.options()$default.args, myArgs))
#install.packages("zoo")
library(zoo)

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

Base_datos.ts = ts(Base_datos, start = 1985, frequency = 4)
Base_datos.ts

plot(Base_datos.ts)

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
#No se generan todas las variables

#Usaré solo una variable 

tcambio_real = data.frame(Base_datos[,1])
precio_cobre = data.frame(Base_datos[,8])

#Esta es la forma correcta para series de tiempo
tcambio_real.ts = ts(Base_datos.ts[,1], start=1985, freq=4)
tcambio_real.ts

#Grafico

#plot(tcambio_real.ts)
plot(tcambio_real.ts, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile")

#Ahora el precio del cobre
precio_cobre.ts = ts(Base_datos.ts[,8], start=1985, freq=4)
precio_cobre.ts

#plot(precio_cobre.ts)
plot(precio_cobre.ts, ylab="Precio", xlab="Trimestres", main="Precio del cobre en Chile")

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

class(Base_datos)
class(Base_datos.ts)

#Primero graficaré su estacionaariedad
seasonplot(precio_cobre.ts, col=rainbow(12), year.labels = TRUE)

#No se entiende mucho porque se grafica la estacionariedad dentro de cada año 

#Observaré la funcuón de autocorrelación a ver si se ve estacionaria
acf(precio_cobre.ts)
#Se aprecia que va disminuyendo lentamente, así se ve una serie no estacionaria

######
#Para convertila a estaconiaria usaremos diferencias
precio_cobre_dif = diff(precio_cobre.ts)
plot(precio_cobre_dif)
#La media es 0, ahora si se ve estacionaria (no es perfecta, pero aceptable)

#Hay un comando para saber cuantas diferencias requiere la serie para ser estacionaria

ndiffs(precio_cobre_dif)

#Ya no necesito diferencias, se genera un 0, ya es estacionaria con la rimera dif

ndiffs(precio_cobre.ts)

#Al aplicarlo sobre la serie sin diferenciar se genera un 1, indicando que hace 
#falta la primera diferencia para logarlo.

# Analisis visual de las graficas de autocorrelacion
par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)
#Le estoy pidiendo que me muestre 1 solo cuadro con 4 gráficas, mfrow es para que me
# muestre 2 columnas y 2 renglones de mi grafica
# Quiero que me muestre primero mi serie original, luego la autocorralacion
# de esa serie no estacionaia, luego que me grefique la serie ya diferenciada y finalmente 
# su respectiva autocorrelacion, para observar las diferenias entre una serie estacionaria y
# una no estacionaria
plot(precio_cobre.ts, ylab="Precio", xlab="Tiempo")
acf(precio_cobre.ts, main="Serie no Estacionaria")
plot(precio_cobre_dif, ylab="Precio", xlab="Tiempo")
acf(precio_cobre_dif, main="Serie Estacionaria")

## Haré un arima 

#Me faltaba una librería

#install.packages("quantmod")
library(quantmod)

#Prueba de Dickey Fuller y Prueba de Ruido Blanco 

adf.test(precio_cobre.ts, alternative = "stationary")

#da un valor p de 0,5098

adf.test(precio_cobre_dif, alternative = "stationary")

#ahora si da un valor p de 0,01 (< 0,05)
#Por ende es estacionaria

#veamos las funciones de autocorrelacion y autocorrelación Parcial
par(mfrow=c(2,1), mar=c(4,4,4,1) + .1)
acf(precio_cobre_dif, main="Función de Auto Correlación")
pacf(precio_cobre_dif, main="Función de Auto Correlación Parcial")

#La función de autocorrelación parcial nos dice el número de autoregresivos,
#La función de autocorrelación nos dice el número de medias moviles

#Para que el rezago coincida con las frecuencias, le damos la sig instrucción

acf(ts(precio_cobre_dif, frequency=1))
pacf(ts(precio_cobre_dif, frequency=1))

#No logro poner titulos}

#Para hacer el modelo arima se usa la serie original, no la diferenciada, se hace 
#un arima 1,1,2 (1 autoregresivos, 1 diferencia, 2 medias moviles)
# REVISAR, NO SE SI CONSIDERAR MÁS REZAGOS
modeloarimacobre = arima(precio_cobre.ts, order=c(1,1,2))
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

#Obtenemos un valor p de 0,94 > 0,05, por lo que hay ruido blanco
# Que el modelo se ajusta bien, significa: que el error tiene media = 0, 
#varianza constante, y no está serialmente correlacionada.

#Ahora veremos el error
error_precio_cobre = residuals(modeloarimacobre)
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
cobreytcambio = cbind(precio_cobre.ts, tcambio_real.ts)
plot(cbind(precio_cobre.ts, tcambio_real.ts), main="Tendencia")

modelocobreytcambio=lm(precio_cobre.ts ~ tcambio_real.ts)
modelocobreytcambio
summary(modelocobreytcambio)
residuales_cobreytcambio = modelocobreytcambio$residuals
summary(residuales_cobreytcambio)

#Mejor forma de hacer 
plot(fitted(modelocobreytcambio), residuales_cobreytcambio)
