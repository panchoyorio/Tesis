#Primero cargamos librerias 

library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate)
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
library(strucchange)
library(quantmod)
library(xts)


#Importamos la base de datos

#file.choose()
ruta_excel <-  "C:\\Users\\fcoyo\\Documents\\GitHub\\Tesis\\Base de Datos Oficial.xlsx"
Base_datos <- read_excel(ruta_excel,
                         sheet = "DATOS.TS")

Base_datos.ts = ts(Base_datos, start = 1985, frequency = 4)
Base_datos.ts
plot(Base_datos.ts, main="Comportamiento de las variables")


#Creo una base sin dummys solo para hacer un grafico bonito
Base_datos_sin_dummys <- read_excel(ruta_excel,
                         sheet = "Datossindummys")

Base_datos_sin_dummys = ts(Base_datos_sin_dummys, start = 1985, frequency = 4)
Base_datos_sin_dummys
plot(Base_datos_sin_dummys, main="Comportamiento de las variables")


#Creo una base solo con las variables que hasta ahora he incluido en el VAR

#plot(ejvar, main="Comportamiento de las variables")

#Separo las variables 

tcambio_real = ts(Base_datos.ts[,1], start=1985, freq=4)
tcambio_real
tcambio_dolarobs = ts(Base_datos.ts[,2], start=1985, freq=4)
tcambio_dolarobs
tasa_TPM = ts(Base_datos.ts[,3], start=1985, freq=4)
tasa_TPM
tasa_captación = ts(Base_datos.ts[,4], start=1985, freq=4)
tasa_captación
tasa_colocación = ts(Base_datos.ts[,5], start=1985, freq=4)
tasa_colocación
tasa_IPC = ts(Base_datos.ts[,6], start=1985, freq=4)
tasa_IPC
tasa_IPC_acum = ts(Base_datos.ts[,7], start=1985, freq=4)
tasa_IPC_acum
precio_cobre = ts(Base_datos.ts[,8], start=1985, freq=4)
precio_cobre
tipo_gob =  ts(Base_datos.ts[,9], start=1985, freq=4)
tipo_gob
crisis_ec =  ts(Base_datos.ts[,10], start=1985, freq=4)
crisis_ec
#Graficos 
plot(tcambio_real, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile", col="blue")
plot(tcambio_dolarobs, ylab="Precio", xlab="Trimestres", main="Valor del dolar observado en Chile", col="blue")
plot(tasa_TPM, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(tasa_captación, ylab="Tasa", xlab="Trimestres", main="Tasa de Captación", col="blue")
plot(tasa_colocación, ylab="Tasa", xlab="Trimestres", main="Tasa de Colocación", col="blue")
plot(tasa_IPC, ylab="Índice", xlab="Trimestres", main="IPC variación mensual", col="blue")
plot(tasa_IPC_acum, ylab="Índice", xlab="Trimestres", main="IPC variación mismo periodo, año anterior", col="blue")
plot(precio_cobre, ylab="Precio", xlab="Trimestres", main="Precio del cobre en Chile", col="blue")


### Gráficos estacionales para el tipo de cambio real, la TPM, y el precio del cobre

seasonplot(tcambio_real, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - Tipo de Cambio Real")
seasonplot(tasa_TPM, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - TPM")
seasonplot(precio_cobre, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - Precio del Cobre")

## Observamos las funciones de autocorrelación de cada variable para ver si son estacionarias

acf(tcambio_real)
acf(tcambio_dolarobs)
acf(tasa_TPM)
acf(tasa_captación)
acf(tasa_colocación)
acf(tasa_IPC)
acf(tasa_IPC_acum)
acf(precio_cobre)
acf(tipo_gob)

##Ninguna de las variables tiene estacionariedad

ndiffs(tcambio_real)
ndiffs(tcambio_dolarobs)
ndiffs(tasa_TPM)
ndiffs(tasa_captación)
ndiffs(tasa_colocación)
ndiffs(tasa_IPC)
ndiffs(tasa_IPC_acum)
ndiffs(precio_cobre)
ndiffs(tipo_gob)
ndiffs(crisis_ec)

### Todas requieren solo de la primera diferencia, exepto la variable crisis (0)


tcambio_real_diff=diff(tcambio_real)
tcambio_dolarobs_diff=diff(tcambio_dolarobs)
tasa_TPM_diff=diff(tasa_TPM)
tasa_captación_diff=diff(tasa_captación)
tasa_colocación_diff=diff(tasa_colocación)
tasa_IPC_diff=diff(tasa_IPC)
tasa_IPC_acum_diff=diff(tasa_IPC_acum)
precio_cobre_diff=diff(precio_cobre)
tipo_gob_diff=diff(tipo_gob)
crisis_ec_diff=diff(crisis_ec)

#Graficamos

#Graficos 

plot(tcambio_real_diff, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile", col="blue")
plot(tcambio_dolarobs_diff, ylab="Precio", xlab="Trimestres", main="Valor del dolar observado en Chile", col="blue")
plot(tasa_TPM_diff, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(tasa_captación_diff, ylab="Tasa", xlab="Trimestres", main="Tasa de Captación", col="blue")
plot(tasa_colocación_diff, ylab="Tasa", xlab="Trimestres", main="Tasa de Colocación", col="blue")
plot(tasa_IPC_diff, ylab="Índice", xlab="Trimestres", main="IPC mensual", col="blue")
plot(tasa_IPC_acum_diff, ylab="Índice", xlab="Trimestres", main="IPC variación año anterior", col="blue")
plot(precio_cobre_diff, ylab="Precio", xlab="Trimestres", main="Precio del cobre en Chile", col="blue")

##Autocorrelaciones

acf(tcambio_real_diff)
acf(tcambio_dolarobs_diff)
acf(tasa_TPM_diff)
acf(tasa_captación_diff)
acf(tasa_colocación_diff)
acf(tasa_IPC_diff)
acf(tasa_IPC_acum_diff)
acf(precio_cobre_diff)

#Comparaciones 

par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)

plot(tcambio_real, ylab="tcambio_real", xlab="Tiempo")
acf(tcambio_real, main="Serie no Estacionaria")
plot(tcambio_real_diff, ylab="Tipo de Cambio Real", xlab="Tiempo")
acf(tcambio_real_diff, main="Serie Estacionaria")

plot(tasa_TPM, ylab="TPM", xlab="Tiempo")
acf(tasa_TPM, main="Serie no Estacionaria")
plot(tasa_TPM_diff, ylab="Tasa de Política Monetaria", xlab="Tiempo")
acf(tasa_TPM_diff, main="Serie Estacionaria")

plot(precio_cobre, ylab="Precio", xlab="Tiempo")
acf(precio_cobre, main="Serie no Estacionaria")
plot(precio_cobre_diff, ylab="Precio del cobre", xlab="Tiempo")
acf(precio_cobre_diff, main="Serie Estacionaria")

#Prueba de Dickey Fuller

adf.test(tcambio_real, alternative = "stationary")
#p-value = 0.6533
adf.test(tcambio_dolarobs, alternative = "stationary")
#p-value = 0.5357
adf.test(tasa_TPM, alternative = "stationary")
#p-value = 0.04151
adf.test(tasa_captación, alternative = "stationary")
# p-value = 0.09471
adf.test(tasa_colocación, alternative = "stationary")
#p-value = 0.07162
adf.test(tasa_IPC, alternative = "stationary")
#p-value = 0.2413
adf.test(tasa_IPC_acum, alternative = "stationary")
#p-value = 0.61111
adf.test(precio_cobre, alternative = "stationary")
#p-value = 0.5098
adf.test(tipo_gob, alternative = "stationary")
#p-value = 0.422
adf.test(crisis_ec, alternative = "stationary")
#p-value = 0,25584

#Para todas las variables se obtuvieron p values > 0,05 a exepción de la TPM
#Esto es raro ya que indica estacionarieda, pero ndiffs me sugiere que se necesita
#una diferencia

adf.test(tcambio_real_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tcambio_dolarobs_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_TPM_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_captación_diff, alternative = "stationary")
# p-value = 0.01
adf.test(tasa_colocación_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_IPC_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_IPC_acum_diff, alternative = "stationary")
#p-value = 0.01
adf.test(precio_cobre_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tipo_gob_diff, alternative = "stationary")
#p-value = 0.01
adf.test(crisis_ec_diff, alternative = "stationary")
#p-value = 0.01

##Todas las variables (diferencias) arrojan pvalue < 0,05
#esto indica que ahora son todas estacionarias

#Funciones de autocorrelacion y autocorrelación Parcial

par(mfrow=c(2,1), mar=c(4,4,4,1) + .1)

acf(tcambio_real_diff, main="Tipo de Cambio Real - Función de Auto Correlación")
pacf(tcambio_real_diff, main="Función de Auto Correlación Parcial")

acf(tcambio_dolarobs_diff, main="Dolar Observado - Función de Auto Correlación")
pacf(tcambio_dolarobs_diff, main="Función de Auto Correlación Parcial")

acf(tasa_TPM_diff, main="TPM - Función de Auto Correlación")
pacf(tasa_TPM_diff, main="Función de Auto Correlación Parcial")

acf(tasa_captación_diff, main="Tasa de Captación - Función de Auto Correlación")
pacf(tasa_captación_diff, main="Función de Auto Correlación Parcial")

acf(tasa_colocación_diff, main="Tasa de Colocación - Función de Auto Correlación")
pacf(tasa_colocación_diff, main="Función de Auto Correlación Parcial")

acf(tasa_IPC_diff, main="IPC mensual - Función de Auto Correlación")
pacf(tasa_IPC_diff, main="Función de Auto Correlación Parcial")

acf(tasa_IPC_acum_diff, main="IPC variación anual - Función de Auto Correlación")
pacf(tasa_IPC_acum_diff, main="Función de Auto Correlación Parcial")

acf(precio_cobre_diff, main="Precio del Cobre - Función de Auto Correlación")
pacf(precio_cobre_diff, main="Función de Auto Correlación Parcial")

#La función de autocorrelación nos dice el número de medias moviles
#La función de autocorrelación parcial nos dice el número de autoregresivos,

#Para que el rezago coincida con las frecuencias, le damos la sig instrucción

acf(ts(precio_cobre_diff, frequency=1))
pacf(ts(precio_cobre_diff, frequency=1))

#Para graficar
par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)

ts.plot(precio_cobre_diff, tcambio_real_diff, tasa_TPM_diff, tasa_captación_diff, tasa_colocación_diff, tasa_IPC_diff, tasa_IPC_acum_diff, tcambio_dolarobs_diff, col=c( "blue", "red", "orange", "yellow", "green", "pink", "black", "brown"))

##El gráfico no se ve bien porque la variable tipo de cambio (real y dolar obs) 
#poseen valores muy altos, usaré logaritmos


logtcambio_real <- log(tcambio_real)
logtcambio_dolarobs <- log(tcambio_dolarobs)
logtasa_TPM <- log(tasa_TPM)
logtasa_captación <- log(tasa_captación)
logtasa_colocación <- log(tasa_colocación)
logtasa_IPC <- log(tasa_IPC)
logtasa_IPC_acum <- log(tasa_IPC_acum)
logprecio_cobre <- log(precio_cobre)

# Warning message:
#   In log(tasa_IPC) : NaNs produced

log_tcambio_real_diff=diff(logtcambio_real)
log_tcambio_dolarobs_diff=diff(logtcambio_dolarobs)
log_tasa_TPM_diff=diff(logtasa_TPM)
log_tasa_captación_diff=diff(logtasa_captación)
log_tasa_colocación_diff=diff(logtasa_colocación)
log_tasa_IPC_diff=diff(logtasa_IPC)
log_tasa_IPC_acum_diff=diff(logtasa_IPC_acum)
log_precio_cobre_diff=diff(logprecio_cobre) 



#Segundo intento

ts.plot(precio_cobre_diff, log_tcambio_real_diff, tasa_TPM_diff, tasa_captación_diff, tasa_colocación_diff, tasa_IPC_diff, tasa_IPC_acum_diff, log_tcambio_dolarobs_diff, col=c( "blue", "red", "orange", "yellow", "green", "pink", "black", "brown"))

#Se usan Logaritmos
ts.plot(log_precio_cobre_diff, log_tcambio_real_diff, log_tasa_TPM_diff, log_tasa_captación_diff, log_tasa_colocación_diff, log_tasa_IPC_diff, log_tasa_IPC_acum_diff, log_tcambio_dolarobs_diff, col=c( "blue", "red", "orange", "yellow", "green", "pink", "black", "brown"))

# El grafico se ve mejor


## Test de causalidad de granger 

# H0: El precio del cobre no es causado en el sentido de granger por el  tipo de cambio real > 0,05
# H1: El precio del cobre sí es causado en el sentido de granger por el  tipo de cambio real < 0,05

grangertest(log_precio_cobre_diff~log_tcambio_real_diff, order = 1)
grangertest(precio_cobre_diff~tcambio_real_diff, order = 1)
# Con la prueba de primer orden nos da 0.005668 **, < 0,05
# (al usar las variables sin log da 0.007238 **)
#Esto indica que se debe rechazar la hipotesis nula, el precio del cobre sí es 
#causado en el sentido de granger por el tipo de cambio real

#Ahora probamos la causalidad en el sentido contrario 

# H0: El tipo de cambio real no es causado en el sentido de granger por el  precio del cobre > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por el  precio del cobre < 0,05

grangertest(log_tcambio_real_diff~log_precio_cobre_diff, order = 1)
grangertest(tcambio_real_diff~precio_cobre_diff, order = 1)

#En este sentido nos da 0.03073 * < 0,05 (al usar las variables sin log da 0.01681 *), 
#por ende tambien se rechaza la hipotesis nula, lo que
#quiere decir que el precio del cobre sí causa al tipo de cambio real

#Probaremos causalidad desde la TPM al tipo de cambio real

# H0: El tipo de cambio real no es causado en el sentido de granger por la TPM > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por la TPM < 0,05


grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 1)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 1)
#Se obtuvo  0.7623 > 0,05, por lo que se acepta la hipotesis nula, no hay causalidad
#0.5273 sin log

#Se hace la prueba de segundo orden
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 2)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 2)
#Se obtiene 0,9 (0,8 sin log), se procede a probar con tercer orden
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 3)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 3)
#Da 0,6, 0,3 sin log
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 4)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 4)
#0.7, 0.4
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 5)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 5)
#0.7, 0.4
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 6)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 6)
#0.6, 0.3
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 7)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 7)
#0.43, 0.18
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 8)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 8)
#0.4, 0.12
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 9)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 9)
#0.46, 0.14
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 10)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 10)
#0.51, 0.19
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 11)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 11)
#0.52, 0.19
grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 12)
grangertest(tcambio_real_diff~tasa_TPM_diff, order = 12)
#0.44, 0.12


###Prueba de granger con las dummy
grangertest(tcambio_real_diff~tipo_gob_diff, order = 1)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 2)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 3)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 4)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 5)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 6)

#Se puede concluir que no hay causalidad para ningun rezago 

# H0:La TPM no es causada en el sentido de granger por el tipo de cambio real > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por la TPM < 0,05

grangertest(log_tasa_TPM_diff~log_tcambio_real_diff, order = 1)
grangertest(tasa_TPM_diff~tcambio_real_diff, order = 1)
#0.21, 0.49
grangertest(log_tasa_TPM_diff~log_tcambio_real_diff, order = 2)
grangertest(tasa_TPM_diff~tcambio_real_diff, order = 2)
#0.053, 0.06
grangertest(log_tasa_TPM_diff~log_tcambio_real_diff, order = 3)
grangertest(tasa_TPM_diff~tcambio_real_diff, order = 3)
#0.01036 *, 0.02614 *

### Se encuentra causalidad desde el tipo de cambio real a la tpm 
# a partir del tercer rezago

# H0: El precio del cobre no es causado en el sentido de granger por la TPM > 0,05
# H1: El precio del cobre real sí es causado en el sentido de granger por la TPM < 0,05

grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 1)
grangertest(precio_cobre_diff~tasa_TPM_diff, order = 1)

## 0.2699, 0.04484 *

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 2)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 2)
#0.6, 0.3

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 3)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 3)
#0.4, 0.5

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 4)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 4)
#0.7, 0.5

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 5)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 5)
#0.8, 0.8
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 6)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 6)
#0.8, 0.6
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 7)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 7)
#0.8, 0.3
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 8)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 8)
#0.9, 0.7 
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 9)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 9)
#0.9, 0.7
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 10)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 10)
#0.8, 0.7
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 11)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 11)
#0.5, 0.5
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 12)
#grangertest(precio_cobre_diff~tasa_TPM_diff, order = 12)
#0.6, 0.6

# Se encuentra evidencia para causalidad desde la TPM al precio del cobre

# H0: La TPM no es causada en el sentido de granger por el precio del cobre > 0,05
# H1: La TPM sí es causada en el sentido de granger por el precio del cobre < 0,05

grangertest(log_tasa_TPM_diff~log_precio_cobre_diff, order = 1)
grangertest(tasa_TPM_diff~precio_cobre_diff, order = 1)
#0.3063, 0.0002759 ***
grangertest(log_tasa_TPM_diff~log_precio_cobre_diff, order = 2)
##0,0004332 ***

#Se encuentra evidencia de causalidad desde el precio del cobre al TPM (segundo orden)

# H0: La TPM no es causada en el sentido de granger por el IPC > 0,05
# H1: La TPM sí es causada en el sentido de granger por el IPC < 0,05

grangertest(tasa_TPM_diff~tasa_IPC_diff, order = 1)
##Se genera un error al sacar logaritmo del IPC, se usan ambas variable sin log
#Entrega un resultado de 0.02035 *, por lo que se rechaza la hip nula

#Sí hay causalidad desde el IPC a la TPM (Tiene sentido puesto que la TPM se hace
#justamente para controlar la inflación medida en el IPC)

# H0: El IPC no es causado en el sentido de granger por la TPM > 0,05
# H1: El IPC sí es causado en el sentido de granger por el TPM < 0,05

grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 1)
# 0,3
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 2)
# 0,9
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 3)
# 0,4
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 4)
# 0,5
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 5)
# 0,3
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 6)
# 0,2
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 7)
# 0,2
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 8)
# 0,2
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 9)
# 0,4
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 10)
# 0,4
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 11)
# 0,2
grangertest(tasa_IPC_diff~tasa_TPM_diff, order = 12)
# 0,2

## Se concluye que no hay causalidad desde la tasa TPM al IPC (por qué lo habría jaja)

# H0: El IPC no es causado en el sentido de granger por el tipo de cambio real > 0,05
# H1: El IPC sí es causado en el sentido de granger por el tipo de cambio real < 0,05

grangertest(tasa_IPC_diff~tcambio_real_diff, order = 1)
#0.002833 **

#si hay causalidad

# H0: El tipo de cambio real no es causado en el sentido de granger por el IPC > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por el IPC < 0,05

grangertest(tcambio_real_diff~tasa_IPC_diff, order = 1)
#0.0237 *

##Si hay causalidad

# H0: El IPC no es causado en el sentido de granger por el Precio del cobre > 0,05
# H1: El IPC sí es causado en el sentido de granger por el Precio del cobre < 0,05

grangertest(tasa_IPC_diff~precio_cobre_diff, order = 1)
# 0.1909
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 2)
#0.09217 .
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 3)
#0.2789
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 4)
#0.0797 .
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 5)
#0.1086
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 6)
#0.1504
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 7)
#0.1155
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 8)
#0.1796
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 9)
#0.2357
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 10)
#0.2476
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 11)
#0.2595
grangertest(tasa_IPC_diff~precio_cobre_diff, order = 12)
#0.1792

## No hay causalidad de granger 


# H0: El precio del cobre no es causado en el sentido de granger por el IPC > 0,05
# H1: El precio del cobre sí es causado en el sentido de granger por el IPC < 0,05

grangertest(precio_cobre_diff~tasa_IPC_diff, order = 1)
#0.3942
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 2)
#0.05366 .
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 3)
#0.06009 .
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 4)
#0.1035
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 5)
#0.2842
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 6)
#0.247
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 7)
#0.1419
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 8)
#0.16
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 9)
#0.2625
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 10)
#0.426
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 11)
#0.5444
grangertest(precio_cobre_diff~tasa_IPC_diff, order = 12)
#0.6467

#No hay causalidad

###PASAMOS AL MODELO VAR
tcambio_real_diff=ts(tcambio_real_diff, start = 1985, freq = 4)
precio_cobre_diff=ts(precio_cobre_diff, start = 1985, freq = 4)
tasa_TPM_diff=ts(tasa_TPM_diff, start = 1985, freq = 4)
tasa_IPC_diff=ts(tasa_IPC_diff, start = 1985, freq = 4)
tcambio_dolarobs_diff=ts(tcambio_dolarobs_diff, start = 1985, freq = 4)
tasa_captación_diff=ts(tasa_captación_diff, start = 1985, freq = 4)
tasa_colocación_diff=ts(tasa_colocación_diff, start = 1985, freq = 4)
tasa_IPC_acum_diff=ts(tasa_IPC_acum_diff, start = 1985, freq = 4)
tipo_gob_diff=ts(tipo_gob_diff, start = 1985, freq = 4)
crisis_ec_diff=ts(crisis_ec_diff, start = 1985, freq = 4)
  
  
ejvar <- cbind(tcambio_real_diff, precio_cobre_diff, tasa_TPM_diff, tasa_IPC_diff)
ejvar

VARselect(ejvar, lag.max = 12)

## 2 de los criterios indican orden 1, los otros 2 indican orden 5
# Partiremos con el orden 1
var1 <- VAR(ejvar, p=1)
var1

summary(var1)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialprecioycambio <- serial.test(var1, lags.pt = 1, type = "PT.asymptotic")
serialprecioycambio$serial

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar1=normality.test(var1)
normvar1$jb.mul

#Sesgo -> p-value = 1.007e-10
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch1 <- arch.test(var1, lags.multi = 1)

## ME TIRÓ UN MONTON DE MENSAJES WARNING (9) DEL ESTILO: 

#Warning messages:
#1: In doTryCatch(return(expr), name, parentenv, handler) :
# display list redraw incomplete   (ERAN TODOS IGUALES)


#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch1$arch.mul
#p-value = 0.04269, esto quiere decir que la varianza de los residuales no es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob=irf(var1, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_pcob

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

plot(irf_pcob)

#Ahora el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables


irf_tcamr=irf(var1, response ="tcambio_real_diff", n.ahead=8, boot=TRUE)
irf_tcamr

plot(irf_tcamr)

#Ahora el impuldo respuesta de la TPM


irf_TPM=irf(var1, response ="tasa_TPM_diff", n.ahead=8, boot=TRUE)
irf_TPM

plot(irf_TPM)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC=irf(var1, response ="tasa_IPC_diff", n.ahead=8, boot=TRUE)
irf_IPC
plot(irf_IPC)

###Impulso respuesta acumulado

irf_pcob_ac=irf(var1, response ="precio_cobre_diff", n.ahead=8, ortho = FALSE,
             cumulative=TRUE)
irf_pcob_ac
plot(irf_pcob_ac)

#Descompocición de la varianza ante una innovación en el precio del cobre
#(le estamos pidiendo 50 observaciones hacia adelante como pronostico)
DESVAR_pcob=fevd(var1, n.ahead=50)$precio_cobre_diff
DESVAR_pcob

#Descompocición de la varianza ante una innovación en el tipo de cambio real
DESVAR_tcamr=fevd(var1, n.ahead=50)$tcambio_real_diff
DESVAR_tcamr
plot(DESVAR_pcob)
#Descompocición de la varianza ante una innovación en la TPM
DESVAR_TPM=fevd(var1, n.ahead=50)$tasa_TPM_diff
DESVAR_TPM


#Descompocición de la varianza ante una innovación en el IPC
DESVAR_IPC=fevd(var1, n.ahead=50)$tasa_IPC_diff
DESVAR_IPC




############################



#VAR 2

ejvar2 <- cbind(tcambio_real_diff, precio_cobre_diff, tasa_TPM_diff, tasa_IPC_diff, tipo_gob_diff, crisis_ec_diff)
VARselect(ejvar2, lag.max = 12)

## 3 de los 4 criterios indican orden 1, 1 indica orden 5
# Partiremos con el orden 1
var2 <- VAR(ejvar2, p=1)
var2

summary(var2)
##Roots of the characteristic polynomial:
## son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var2)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar2 <- serial.test(var2, lags.pt = 1, type = "PT.asymptotic")
serialvar2$serial

## Obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar2=normality.test(var2)
normvar2$jb.mul

#Sesgo -> p-value = 4.932e-13
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch2 <- arch.test(var2, lags.multi = 1)

## ME TIRÓ UN MONTON DE MENSAJES WARNING (9) DEL ESTILO: 

#Warning messages:
#1: In doTryCatch(return(expr), name, parentenv, handler) :
# display list redraw incomplete   (ERAN TODOS IGUALES)


#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch2$arch.mul
#p-value = 4.552e-08, esto quiere decir que la varianza de los residuales no es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_2pcob=irf(var2, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_2pcob

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

plot(irf_2pcob)

#Ahora el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables


irf_2tcamr=irf(var2, response ="tcambio_real_diff", n.ahead=8, boot=TRUE)
irf_2tcamr

plot(irf_2tcamr)

#Ahora el impulso respuesta de la TPM


irf_2TPM=irf(var2, response ="tasa_TPM_diff", n.ahead=8, boot=TRUE)
irf_2TPM

plot(irf_2TPM)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_2IPC=irf(var2, response ="tasa_IPC_diff", n.ahead=8, boot=TRUE)
irf_2IPC
plot(irf_2IPC)


#VAR 3 #### QUERIAMOS HACER UN VAR CON LA TPM SIN DIFERENCIAR, EL DICKEY FULLER
##FUE DE 0,04, PERO AL INTENTARLO OBTENEMOS UN ERROR: NAs in y.

ejvar3 <- cbind(tcambio_real_diff, precio_cobre_diff, tasa_TPM, tasa_IPC_diff, tipo_gob_diff, crisis_ec_diff)
VARselect(ejvar3, lag.max = 12)

#####

#VAR 4 ##Se repite VAR1 pero con mas rezagos

# VARSELECT indica orden 5
 
var4 <- VAR(ejvar2, p=5)
var4

summary(var4)
##Roots of the characteristic polynomial:
## son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var4)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar4 <- serial.test(var4, lags.pt = 1, type = "PT.asymptotic")
serialvar4$serial
#Obtenemos : Warning messages:
#1: In pchisq(STATISTIC, df = PARAMETER) : NaNs produced
#2: In pchisq(STATISTIC, df = PARAMETER) : NaNs produced

#El test de premanteau dio:

## p-value = NA

###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar4=normality.test(var4)
normvar4$jb.mul

#Sesgo -> p-value = 3.403e-09
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch4 <- arch.test(var4, lags.multi = 5)

## ME TIRÓ UN MONTON DE MENSAJES WARNING (9) DEL ESTILO: 

#Warning messages:
#1: In doTryCatch(return(expr), name, parentenv, handler) :
# display list redraw incomplete   (ERAN TODOS IGUALES)


#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch4$arch.mul
#p-value = 0.3149, esto quiere decir que la varianza de los residuales sí es constante
##YIIIIEI

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_4pcob=irf(var4, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_4pcob

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

plot(irf_4pcob)

#Ahora el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables


irf_4tcamr=irf(var4, response ="tcambio_real_diff", n.ahead=8, boot=TRUE)
irf_4tcamr

plot(irf_4tcamr)

#Ahora el impulso respuesta de la TPM


irf_4TPM=irf(var4, response ="tasa_TPM_diff", n.ahead=8, boot=TRUE)
irf_4TPM

plot(irf_4TPM)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_4IPC=irf(var4, response ="tasa_IPC_diff", n.ahead=8, boot=TRUE)
irf_4IPC
plot(irf_2IPC)


############ EL MODELO VAR (VAR REDUCIDO) NO TIENE EN CUENTA LA DEPENDENCIA ACTUAL, 
# YA QUE TODAS SUS VARIABLES DEPENDEN DE SUS PROPIOS VALORES PASADOS Y DE 
# LOS VALORES PASADOS DE LAS DEMÁS VARIABLES

#### LA DEPENDENCIA ACTUAL ES UN COMPONENTE QUE SE AÑADE, EN LOS MODELOS SVAR
## VECTORES AUTOREGRESIVOS ESTRUCTURALES (SVAR) ###
# SVAR

