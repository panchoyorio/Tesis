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
library(Hmisc)

#Importamos la base de datos

#file.choose()
ruta_excel <-  "C:\\Users\\fcoyo\\Documents\\GitHub\\Tesis\\Base de Datos Oficial.xlsx"
Base_datos <- read_excel(ruta_excel,
                         sheet = "DATOS.TS")

Base_datos = ts(Base_datos, start = 1986, frequency = 4)
Base_datos
plot(Base_datos, main="Comportamiento de las variables")


#Creo una base sin dummys solo para hacer un grafico bonito
Base_graficada_1 <- read_excel(ruta_excel,
                         sheet = "Base_graficada_1")

Base_graficada_1= ts(Base_graficada_1, start = 1986, frequency = 4)
Base_graficada_1
plot(Base_graficada_1, main="Comportamiento de las variables")


####Grafico con PIB de cada servicio en vez de valor añadido
Base_graficada_2 <- read_excel(ruta_excel,
                               sheet = "Base_graficada_2")

Base_graficada_2= ts(Base_graficada_2, start = 1986, frequency = 4)
Base_graficada_2
plot(Base_graficada_2, main="Comportamiento de las variables")

#Creo una base solo con las variables que hasta ahora he incluido en el VAR

#plot(ejvar, main="Comportamiento de las variables")

#Separo las variables 

tcambio_real = ts(Base_datos[,1], start=1986, freq=4)
tcambio_real
tcambio_dolarobs = ts(Base_datos[,2], start=1986, freq=4)
tcambio_dolarobs
TPM = ts(Base_datos[,3], start=1986, freq=4)
TPM
tasa_captación = ts(Base_datos[,4], start=1986, freq=4)
tasa_captación
tasa_colocación = ts(Base_datos[,5], start=1986, freq=4)
tasa_colocación
IPC = ts(Base_datos[,6], start=1986, freq=4)
IPC
IPC_acum = ts(Base_datos[,7], start=1986, freq=4)
IPC_acum
tasa_desempleo = ts(Base_datos[,8], start=1986, freq=4)
tasa_desempleo
precio_cobre = ts(Base_datos[,9], start=1986, freq=4)
precio_cobre
pib_manufacturas =  ts(Base_datos[,10], start=1986, freq=4)
pib_manufacturas
pib_servicios =  ts(Base_datos[,11], start=1986, freq=4)
pib_servicios
pib_minería =  ts(Base_datos[,12], start=1986, freq=4)
pib_minería
VA_manufacturas =  ts(Base_datos[,13], start=1986, freq=4)
VA_manufacturas
VA_servicios =  ts(Base_datos[,14], start=1986, freq=4)
VA_servicios
VA_minería =  ts(Base_datos[,15], start=1986, freq=4)
VA_minería
tipo_gob =  ts(Base_datos[,16], start=1986, freq=4)
tipo_gob
crisis_ec =  ts(Base_datos[,17], start=1986, freq=4)
crisis_ec
#Graficos 
plot(tcambio_real, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile", col="blue")
plot(tcambio_dolarobs, ylab="Precio", xlab="Trimestres", main="Valor del dolar observado en Chile", col="blue")
plot(TPM, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(tasa_captación, ylab="Tasa", xlab="Trimestres", main="Tasa de Captación", col="blue")
plot(tasa_colocación, ylab="Tasa", xlab="Trimestres", main="Tasa de Colocación", col="blue")
plot(IPC, ylab="Índice", xlab="Trimestres", main="IPC variación mensual", col="blue")
plot(IPC_acum, ylab="Índice", xlab="Trimestres", main="IPC variación mismo periodo, año anterior", col="blue")
plot(tasa_desempleo, ylab="Tasa", xlab="Trimestres", main="Tasa de desempleo", col="blue")
plot(precio_cobre, ylab="Precio", xlab="Trimestres", main="Precio del cobre en Chile", col="blue")
plot(pib_manufacturas, ylab="PIB", xlab="Sector Manufacturas", col="blue")
plot(pib_servicios, ylab="PIB", xlab="Sector Servicios", col="blue")
plot(pib_minería, ylab="PIB", xlab="Sector Minería", col="blue")
plot(VA_manufacturas, ylab="Porcentaje del PIB", xlab="Valor Añadido del Sector Manufacturas", col="blue")
plot(VA_servicios, ylab="Porcentaje del PIB", xlab="Valor Añadido del Sector Servicios", col="blue")
plot(VA_minería, ylab="Rentas Mineras como Porcentaje del PIB", xlab="Valor Añadido del Sector Minería", col="blue")

### Algunos interesantes

plot(cbind(precio_cobre, tcambio_dolarobs), main="Precio del Cobre - Dolar Observado", col="blue")
plot(cbind(precio_cobre, VA_manufacturas), main="Precio del Cobre - Valor Añadido Manufacturas", col="blue")
plot(cbind(precio_cobre, VA_servicios), main="Precio del Cobre - Valor Añadido Servicios", col="blue")
plot(cbind(precio_cobre, VA_minería), main="Precio del Cobre - Valor Añadido Minería", col="blue")
plot(cbind(precio_cobre, pib_manufacturas), main="Precio del Cobre - PIB Manufacturas", col="blue")
plot(cbind(precio_cobre, pib_servicios), main="Precio del Cobre -PIB Servicios", col="blue")
plot(cbind(precio_cobre, pib_minería), main="Precio del Cobre - PIB Minería", col="blue")
plot(cbind(VA_manufacturas, pib_manufacturas), main="Valor Añadido - Pib Manufacturas", col="blue")
plot(cbind(VA_manufacturas, VA_minería), main="Valor Añadido Manufacturas - Minería", col="blue")
ts.plot(precio_cobre, VA_manufacturas, main="Precio del Cobre - Valor Añadido Manufacturas", col=c( "blue", "red"))
ts.plot(VA_minería, VA_manufacturas, main="Valor Añadido Minería - Manufacturas", col=c( "blue", "red"))


### Gráficos estacionales para el tipo de cambio real, la TPM, y el precio del cobre

seasonplot(tcambio_dolarobs, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - Tipo de Cambio Real")
seasonplot(TPM, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - TPM")
seasonplot(precio_cobre, col=rainbow(12), year.labels = TRUE, main = "Grafico Estacional - Precio del Cobre")

## Observamos las funciones de autocorrelación de cada variable para ver si son estacionarias

acf(tcambio_real)
acf(tcambio_dolarobs)
acf(TPM)
acf(tasa_captación)
acf(tasa_colocación)
acf(IPC)
acf(IPC_acum)
acf(tasa_desempleo)
acf(precio_cobre)
acf(pib_manufacturas)
acf(pib_servicios)
acf(pib_minería)
acf(VA_manufacturas)
acf(VA_servicios)
acf(VA_minería)
acf(tipo_gob)
acf(crisis_ec)

##Ninguna de las variables tiene estacionariedad

ndiffs(tcambio_real)
ndiffs(tcambio_dolarobs)
ndiffs(TPM)
ndiffs(tasa_captación)
ndiffs(tasa_colocación)
ndiffs(IPC)
ndiffs(IPC_acum)
ndiffs(tasa_desempleo)
ndiffs(precio_cobre)
ndiffs(pib_manufacturas)
ndiffs(pib_servicios)
ndiffs(pib_minería)
ndiffs(VA_manufacturas)
ndiffs(VA_servicios)
ndiffs(VA_minería)
ndiffs(tipo_gob)
ndiffs(crisis_ec)


### RESULTADOS: LA MAYORÍA REQUIERE PRIMERA DIFERENCIA, EL PIB SERVICIOS
#NECESITA 2, crisis 0

####### Diferencias

tcambio_real_diff=diff(tcambio_real)
tcambio_dolarobs_diff=diff(tcambio_dolarobs)
TPM_diff=diff(TPM)
tasa_captación_diff=diff(tasa_captación)
tasa_colocación_diff=diff(tasa_colocación)
IPC_diff=diff(IPC)
IPC_acum_diff=diff(IPC_acum)
tasa_desempleo_diff=diff(tasa_desempleo)
precio_cobre_diff=diff(precio_cobre)
pib_manufacturas_diff=diff(pib_manufacturas)
pib_servicios_diff=diff(pib_servicios)
pib_minería_diff=diff(pib_minería)
VA_manufacturas_diff=diff(VA_manufacturas)
VA_servicios_diff=diff(VA_servicios)
VA_minería_diff=diff(VA_minería)
tipo_gob_diff=diff(tipo_gob)
crisis_ec_diff=diff(crisis_ec)


ndiffs(pib_servicios_diff)
##Aún falta una diferencia
pib_servicios_diff2=diff(pib_servicios_diff)


##Creo una base en data frame con las variables diferenciadas
##Elimino los NA

Base_datos2 <- cbind(tcambio_real_diff, tcambio_dolarobs_diff, IPC_diff, IPC_acum_diff, tasa_desempleo_diff, TPM_diff, pib_manufacturas_diff, pib_servicios_diff2, pib_minería_diff, VA_manufacturas_diff, VA_servicios_diff, VA_minería_diff, precio_cobre_diff, tipo_gob_diff, crisis_ec)

Base_datos2<-data.frame(Base_datos2)

Base_datos2$nuevo_pib_servicios_diff2<- Lag(Base_datos2$pib_servicios_diff2, -1)
ejvar

grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 1)






#Prueba de Dickey Fuller

adf.test(tcambio_real, alternative = "stationary")
#p-value = 0.6533
adf.test(tcambio_dolarobs, alternative = "stationary")
#p-value = 0.5356
adf.test(TPM, alternative = "stationary")
#p-value = 0.04617
adf.test(tasa_captación, alternative = "stationary")
# p-value = 0.09471
adf.test(tasa_colocación, alternative = "stationary")
#p-value = 0.07162
adf.test(IPC, alternative = "stationary")
#p-value = 0.2413
adf.test(IPC_acum, alternative = "stationary")
#p-value = 0.61111
adf.test(tasa_desempleo, alternative = "stationary")
#p-value = 0.3831
adf.test(precio_cobre, alternative = "stationary")
#p-value = 0.5098
adf.test(pib_manufacturas, alternative = "stationary")
#p-value = 0.6332
adf.test(pib_servicios, alternative = "stationary")
#p-value = 0.9124
adf.test(pib_minería, alternative = "stationary")
#p-value = 0.3102
adf.test(VA_manufacturas, alternative = "stationary")
#p-value = 0.5559
adf.test(VA_servicios, alternative = "stationary")
#p-value = 0.3611
adf.test(VA_minería, alternative = "stationary")
#p-value = 0.4951
adf.test(tipo_gob, alternative = "stationary")
#p-value = 0.422
adf.test(crisis_ec, alternative = "stationary")
#p-value = 0,25584

#Para todas las variables se obtuvieron p values > 0,05 (Excepto TPM)

adf.test(tcambio_real_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tcambio_dolarobs_diff, alternative = "stationary")
#p-value = 0.01239
adf.test(TPM_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_captación_diff, alternative = "stationary")
# p-value = 0.01
adf.test(tasa_colocación_diff, alternative = "stationary")
#p-value = 0.01
adf.test(IPC_diff, alternative = "stationary")
#p-value = 0.01
adf.test(IPC_acum_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tasa_desempleo_diff, alternative = "stationary")
#p-value = 0.01
adf.test(precio_cobre_diff, alternative = "stationary")
#p-value = 0.01
adf.test(pib_manufacturas_diff, alternative = "stationary")
#p-value = 0.01
adf.test(pib_servicios_diff, alternative = "stationary")
#p-value = 0.01
adf.test(pib_minería_diff, alternative = "stationary")
#p-value = 0.01
adf.test(VA_manufacturas_diff, alternative = "stationary")
#p-value = 0.0945
adf.test(VA_servicios_diff, alternative = "stationary")
#p-value = 0.01
adf.test(VA_minería_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tipo_gob_diff, alternative = "stationary")
#p-value = 0.01
adf.test(crisis_ec_diff, alternative = "stationary")
#p-value = 0.01

ndiffs(VA_manufacturas_diff)

###Debemos elegir un criterio para medir la estacionariedad, podemos guiarnos por
#la interpreteción de los resultados de la prueba ndiffs (la cual se basa en las 
#pruebas kpps, adf y pp) o direnctamente utilizar los dickey fuller (adf); En el primer 
#caso señala que se debe aplicar segunda diferencia al pib servicios, en el segundo caso
# señala que se debe aplicar segunda diff a valor añadido en manufacturas, 

#Se realiza segunda diff
VA_manufacturas_diff2=diff(VA_manufacturas_diff)

adf.test(VA_manufacturas_diff2, alternative = "stationary")
#p-value = 0.01


###Graficamos


plot(tcambio_real_diff, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real", col="blue")
plot(tcambio_dolarobs_diff, ylab="Precio", xlab="Trimestres", main="Dolar Observado", col="blue")
plot(TPM_diff, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(tasa_captación_diff, ylab="Tasa", xlab="Trimestres", main="Tasa de Captación", col="blue")
plot(tasa_colocación_diff, ylab="Tasa", xlab="Trimestres", main="Tasa de Colocación", col="blue")
plot(IPC_diff, ylab="Índice", xlab="Trimestres", main="IPC general", col="blue")
plot(IPC_acum_diff, ylab="Índice", xlab="Trimestres", main="IPC variación año anterior", col="blue")
plot(tasa_desempleo_diff, ylab="Tasa", xlab="Trimestres", main="Tasa de Desempleo", col="blue")
plot(precio_cobre_diff, ylab="Precio", xlab="Trimestres", main="Precio del Cobre", col="blue")
plot(pib_manufacturas_diff, ylab="PIB", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(pib_servicios_diff, ylab="PIB", xlab="Trimestres", main="Sector Servicios (1dif)", col="blue")
plot(pib_servicios_diff2, ylab="PIB", xlab="Trimestres", main="Sector Servicios (2dif)", col="blue")
plot(pib_minería_diff, ylab="PIB", xlab="Trimestres", main="Sector Minero", col="blue")
plot(VA_manufacturas_diff2, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(VA_servicios_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Servicios", col="blue")
plot(VA_minería_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Minero", col="blue")


##Autocorrelaciones

acf(tcambio_real_diff)
acf(tcambio_dolarobs_diff)
acf(TPM_diff)
acf(tasa_captación_diff)
acf(tasa_colocación_diff)
acf(IPC_diff)
acf(IPC_acum_diff)
acf(tasa_desempleo_diff)
acf(precio_cobre_diff)
acf(pib_manufacturas_diff)
acf(pib_servicios_diff)
acf(pib_servicios_diff2)
acf(pib_minería_diff)
acf(VA_manufacturas_diff)
acf(VA_servicios_diff)
acf(VA_minería_diff)

#Comparaciones 

par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)

plot(tcambio_dolarobs, ylab="Dolar Observado", xlab="Tiempo")
acf(tcambio_dolarobs, main="Serie no Estacionaria")
plot(tcambio_dolarobs_diff, ylab="D.O", xlab="Tiempo")
acf(tcambio_dolarobs_diff, main="Serie Estacionaria")

plot(IPC, ylab="IPC", xlab="Tiempo")
acf(IPC, main="Serie no Estacionaria")
plot(IPC_diff, ylab="IPC", xlab="Tiempo")
acf(IPC_diff, main="Serie Estacionaria")

plot(precio_cobre, ylab="Precio", xlab="Tiempo")
acf(precio_cobre, main="Serie no Estacionaria")
plot(precio_cobre_diff, ylab="Precio del cobre", xlab="Tiempo")
acf(precio_cobre_diff, main="Serie Estacionaria")


#Funciones de autocorrelacion y autocorrelación Parcial

par(mfrow=c(2,1), mar=c(4,4,4,1) + .1)

acf(tcambio_real_diff, main="Tipo de Cambio Real - Función de Auto Correlación")
pacf(tcambio_real_diff, main="Función de Auto Correlación Parcial")

acf(tcambio_dolarobs_diff, main="Dolar Observado - Función de Auto Correlación")
pacf(tcambio_dolarobs_diff, main="Función de Auto Correlación Parcial")

acf(TPM_diff, main="TPM - Función de Auto Correlación")
pacf(TPM_diff, main="Función de Auto Correlación Parcial")

acf(tasa_captación_diff, main="Tasa de Captación - Función de Auto Correlación")
pacf(tasa_captación_diff, main="Función de Auto Correlación Parcial")

acf(tasa_colocación_diff, main="Tasa de Colocación - Función de Auto Correlación")
pacf(tasa_colocación_diff, main="Función de Auto Correlación Parcial")

acf(IPC_diff, main="IPC mensual - Función de Auto Correlación")
pacf(IPC_diff, main="Función de Auto Correlación Parcial")

acf(IPC_acum_diff, main="IPC variación anual - Función de Auto Correlación")
pacf(IPC_acum_diff, main="Función de Auto Correlación Parcial")

acf(tasa_desempleo_diff, main="Tasa de Desempleo - Función de Auto Correlación")
pacf(tasa_desempleo_diff, main="Función de Auto Correlación Parcial")

acf(precio_cobre_diff, main="Precio del Cobre - Función de Auto Correlación")
pacf(precio_cobre_diff, main="Función de Auto Correlación Parcial")

acf(pib_manufacturas_diff, main="PIB manufacturero - Función de Auto Correlación")
pacf(pib_manufacturas_diff, main="Función de Auto Correlación Parcial")

acf(pib_servicios_diff, main="PIB servicios - Función de Auto Correlación")
pacf(pib_servicios_diff, main="Función de Auto Correlación Parcial")

acf(pib_minería_diff, main="PIB minería - Función de Auto Correlación")
pacf(pib_minería_diff, main="Función de Auto Correlación Parcial")

acf(VA_manufacturas_diff, main="Valor Añadido Manufacturas - Función de Auto Correlación")
pacf(VA_manufacturas_diff, main="Función de Auto Correlación Parcial")

acf(VA_manufacturas_diff2, main="Valor Añadido Manufacturas - Función de Auto Correlación")
pacf(VA_manufacturas_diff2, main="Función de Auto Correlación Parcial")

acf(VA_servicios_diff, main="Valor Añadido Servicios - Función de Auto Correlación")
pacf(VA_servicios_diff, main="Función de Auto Correlación Parcial")

acf(VA_minería_diff, main="Valor Añadido Minería - Función de Auto Correlación")
pacf(VA_minería_diff, main="Función de Auto Correlación Parcial")

#La función de autocorrelación nos dice el número de medias moviles
#La función de autocorrelación parcial nos dice el número de autoregresivos,

#Para que el rezago coincida con las frecuencias, le damos la sig instrucción

#acf(ts(precio_cobre_diff, frequency=1))
#pacf(ts(precio_cobre_diff, frequency=1))

#Para graficar
par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)


# El grafico se ve mejor


## Test de causalidad de granger 

# H0: El precio del cobre no es causado en el sentido de granger por el  tipo de cambio real > 0,05
# H1: El precio del cobre sí es causado en el sentido de granger por el  tipo de cambio real < 0,05

#grangertest(log_precio_cobre_diff~log_tcambio_real_diff, order = 1)
grangertest(precio_cobre_diff~tcambio_real_diff, order = 1)
# Con la prueba de primer orden nos da 0.005668 **, < 0,05
# (al usar las variables sin log da 0.007238 **)
#Esto indica que se debe rechazar la hipotesis nula, el precio del cobre sí es 
#causado en el sentido de granger por el tipo de cambio real

#Ahora probamos la causalidad en el sentido contrario 

# H0: El tipo de cambio real no es causado en el sentido de granger por el  precio del cobre > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por el  precio del cobre < 0,05

#grangertest(log_tcambio_real_diff~log_precio_cobre_diff, order = 1)
grangertest(tcambio_real_diff~precio_cobre_diff, order = 1)

#En este sentido nos da 0.03073 * < 0,05 (al usar las variables sin log da 0.01681 *), 
#por ende tambien se rechaza la hipotesis nula, lo que
#quiere decir que el precio del cobre sí causa al tipo de cambio real

# H0: El dólar observado no es causado en el sentido de granger por el  precio del cobre > 0,05
# H1: El dólar observado sí es causado en el sentido de granger por el precio del cobre < 0,05


grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 1)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 2)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 3)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 4)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 5)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 6)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 7)
grangertest(tcambio_dolarobs_diff~precio_cobre_diff, order = 8)
##0.03061 *

# H0: El precio del cobre no es causado en el sentido de granger por el  dólar observado > 0,05
# H1: El precio del cobre sí es causado en el sentido de granger por el  dólar observado < 0,05

grangertest(precio_cobre_diff~tcambio_dolarobs_diff, order = 1)
#0.002209 **

#Probaremos causalidad desde la TPM al tipo de cambio real

# H0: El tipo de cambio real no es causado en el sentido de granger por la TPM > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por la TPM < 0,05


#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 1)
grangertest(tcambio_real_diff~TPM_diff, order = 1)
#Se obtuvo  0.7623 > 0,05, por lo que se acepta la hipotesis nula, no hay causalidad
# 0.6638 sin log

#Se hace la prueba de segundo orden
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 2)
grangertest(tcambio_real_diff~TPM_diff, order = 2)
#Se obtiene 0,9 (0,8 sin log), se procede a probar con tercer orden
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 3)
grangertest(tcambio_real_diff~TPM_diff, order = 3)
#Da 0,6, 0,3 sin log
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 4)
grangertest(tcambio_real_diff~TPM_diff, order = 4)
#0.7, 0.4
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 5)
grangertest(tcambio_real_diff~TPM_diff, order = 5)
#0.7, 0.4
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 6)
grangertest(tcambio_real_diff~TPM_diff, order = 6)
#0.6, 0.3
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 7)
grangertest(tcambio_real_diff~TPM_diff, order = 7)
#0.43, 0.18
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 8)
grangertest(tcambio_real_diff~TPM_diff, order = 8)
#0.4, 0.12
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 9)
grangertest(tcambio_real_diff~TPM_diff, order = 9)
#0.46, 0.14
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 10)
grangertest(tcambio_real_diff~TPM_diff, order = 10)
#0.51, 0.19
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 11)
grangertest(tcambio_real_diff~TPM_diff, order = 11)
#0.52, 0.05483
#grangertest(log_tcambio_real_diff~log_tasa_TPM_diff, order = 12)
grangertest(tcambio_real_diff~TPM_diff, order = 12)
#0.44, 0.04879

# H0:La TPM no es causada en el sentido de granger por el tipo de cambio real > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por la TPM < 0,05

#grangertest(log_tasa_TPM_diff~log_tcambio_real_diff, order = 1)
grangertest(TPM_diff~tcambio_real_diff, order = 1)
#0.742
#grangertest(log_tasa_TPM_diff~log_tcambio_real_diff, order = 2)
grangertest(TPM_diff~tcambio_real_diff, order = 2)
#0.0228*

### Se encuentra causalidad desde el tipo de cambio real a la tpm 
# a partir del 2° rezago


###Prueba de granger con las dummy

# H0: El tipo de cambio real no es causado en el sentido de granger por el tipo de gobierno > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por el tipo de gobierno < 0,05


grangertest(tcambio_real_diff~tipo_gob_diff, order = 1)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 2)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 3)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 4)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 5)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 6)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 7)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 8)
grangertest(tcambio_real_diff~tipo_gob_diff, order = 9)
#0.04816 *


#Se puede concluir que hay causalidad para el 9° rezago

# H0: El tipo de gobierno no es causado en el sentido de granger por el TCR > 0,05
# H1: El tipo de gobierno sí es causado en el sentido de granger por EL TCR < 0,05

grangertest(tipo_gob_diff~tcambio_real_diff, order = 1)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 2)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 3)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 4)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 5)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 6)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 7)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 8)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 9)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 10)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 11)
grangertest(tipo_gob_diff~tcambio_real_diff, order = 12)

## No hay causalidad


# H0: El precio del cobre no es causado en el sentido de granger por la TPM > 0,05
# H1: El precio del cobre real sí es causado en el sentido de granger por la TPM < 0,05

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 1)
grangertest(precio_cobre_diff~TPM_diff, order = 1)

#0.06063 *

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 2)
grangertest(precio_cobre_diff~TPM_diff, order = 2)
#0.375

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 3)
grangertest(precio_cobre_diff~TPM_diff, order = 3)
#0.51

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 4)
grangertest(precio_cobre_diff~TPM_diff, order = 4)
#0.7, 0.5

#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 5)
grangertest(precio_cobre_diff~TPM_diff, order = 5)
#0.8, 0.8
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 6)
grangertest(precio_cobre_diff~TPM_diff, order = 6)
#0.8, 0.5
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 7)
grangertest(precio_cobre_diff~TPM_diff, order = 7)
#0.8, 0.21
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 8)
grangertest(precio_cobre_diff~TPM_diff, order = 8)
#0.9, 0.59 
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 9)
grangertest(precio_cobre_diff~TPM_diff, order = 9)
#0.9, 0.65
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 10)
grangertest(precio_cobre_diff~TPM_diff, order = 10)
#0.8, 0.7
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 11)
grangertest(precio_cobre_diff~TPM_diff, order = 11)
#0.5, 0.63
#grangertest(log_precio_cobre_diff~log_tasa_TPM_diff, order = 12)
grangertest(precio_cobre_diff~TPM_diff, order = 12)
#0.6, 0.6137

# No se encuentra evidencia para causalidad desde la TPM al precio del cobre

# H0: La TPM no es causada en el sentido de granger por el precio del cobre > 0,05
# H1: La TPM sí es causada en el sentido de granger por el precio del cobre < 0,05

#grangertest(log_tasa_TPM_diff~log_precio_cobre_diff, order = 1)
grangertest(TPM_diff~precio_cobre_diff, order = 1)
# 0.001377 **

#Se encuentra evidencia de causalidad desde el precio del cobre al TPM 

# H0: La TPM no es causada en el sentido de granger por el IPC > 0,05
# H1: La TPM sí es causada en el sentido de granger por el IPC < 0,05

grangertest(TPM_diff~IPC_diff, order = 1)
##Se genera un error al sacar logaritmo del IPC, se usan ambas variable sin log
#Entrega un resultado de 0.03232 *, por lo que se rechaza la hip nula

#Sí hay causalidad desde el IPC a la TPM (Tiene sentido puesto que la TPM se hace
#justamente para controlar la inflación medida en el IPC)

# H0: El IPC no es causado en el sentido de granger por la TPM > 0,05
# H1: El IPC sí es causado en el sentido de granger por el TPM < 0,05

grangertest(IPC_diff~TPM_diff, order = 1)
# 0,3
grangertest(IPC_diff~TPM_diff, order = 2)
# 0,9
grangertest(IPC_diff~TPM_diff, order = 3)
# 0,4
grangertest(IPC_diff~TPM_diff, order = 4)
# 0,5
grangertest(IPC_diff~TPM_diff, order = 5)
# 0,3
grangertest(IPC_diff~TPM_diff, order = 6)
# 0,2
grangertest(IPC_diff~TPM_diff, order = 7)
# 0,2
grangertest(IPC_diff~TPM_diff, order = 8)
# 0,2
grangertest(IPC_diff~TPM_diff, order = 9)
# 0,4
grangertest(IPC_diff~TPM_diff, order = 10)
# 0,4
grangertest(IPC_diff~TPM_diff, order = 11)
# 0,2
grangertest(IPC_diff~TPM_diff, order = 12)
# 0,2

## Se concluye que no hay causalidad desde la tasa TPM al IPC (por qué lo habría jaja)

# H0: El IPC no es causado en el sentido de granger por el tipo de cambio real > 0,05
# H1: El IPC sí es causado en el sentido de granger por el tipo de cambio real < 0,05

grangertest(IPC_diff~tcambio_real_diff, order = 1)
#0.002833 **

#si hay causalidad

# H0: El tipo de cambio real no es causado en el sentido de granger por el IPC > 0,05
# H1: El tipo de cambio real sí es causado en el sentido de granger por el IPC < 0,05

grangertest(tcambio_real_diff~IPC_diff, order = 1)
#0.0237 *

##Si hay causalidad

# H0: El IPC no es causado en el sentido de granger por el Precio del cobre > 0,05
# H1: El IPC sí es causado en el sentido de granger por el Precio del cobre < 0,05

grangertest(IPC_diff~precio_cobre_diff, order = 1)
# 0.1909
grangertest(IPC_diff~precio_cobre_diff, order = 2)
#0.09217 .
grangertest(IPC_diff~precio_cobre_diff, order = 3)
#0.2789
grangertest(IPC_diff~precio_cobre_diff, order = 4)
#0.0797 .
grangertest(IPC_diff~precio_cobre_diff, order = 5)
#0.1086
grangertest(IPC_diff~precio_cobre_diff, order = 6)
#0.1504
grangertest(IPC_diff~precio_cobre_diff, order = 7)
#0.1155
grangertest(IPC_diff~precio_cobre_diff, order = 8)
#0.1796
grangertest(IPC_diff~precio_cobre_diff, order = 9)
#0.2357
grangertest(IPC_diff~precio_cobre_diff, order = 10)
#0.2476
grangertest(IPC_diff~precio_cobre_diff, order = 11)
#0.2595
grangertest(IPC_diff~precio_cobre_diff, order = 12)
#0.1792

## No hay causalidad de granger desde el precio del cobre hacia el IPC


# H0: El precio del cobre no es causado en el sentido de granger por el IPC > 0,05
# H1: El precio del cobre sí es causado en el sentido de granger por el IPC < 0,05

grangertest(precio_cobre_diff~IPC_diff, order = 1)
#0.3942
grangertest(precio_cobre_diff~IPC_diff, order = 2)
#0.05366 .
grangertest(precio_cobre_diff~IPC_diff, order = 3)
#0.06009 .
grangertest(precio_cobre_diff~IPC_diff, order = 4)
#0.1035
grangertest(precio_cobre_diff~IPC_diff, order = 5)
#0.2842
grangertest(precio_cobre_diff~IPC_diff, order = 6)
#0.247
grangertest(precio_cobre_diff~IPC_diff, order = 7)
#0.1419
grangertest(precio_cobre_diff~IPC_diff, order = 8)
#0.16
grangertest(precio_cobre_diff~IPC_diff, order = 9)
#0.2625
grangertest(precio_cobre_diff~IPC_diff, order = 10)
#0.426
grangertest(precio_cobre_diff~IPC_diff, order = 11)
#0.5444
grangertest(precio_cobre_diff~IPC_diff, order = 12)
#0.6467

#No hay causalidad

# H0: La tasa de desempleo no es causada en el sentido de granger por el precio del cobre > 0,05
# H1: La tasa de desempleo sí es causada en el sentido de granger por el precio del cobre < 0,05

grangertest(tasa_desempleo_diff~precio_cobre_diff, order = 1)
grangertest(tasa_desempleo_diff~precio_cobre_diff, order = 2)
#0.02944 *

## AL REVES
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 1)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 2)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 3)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 4)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 5)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 6)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 7)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 8)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 9)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 10)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 11)
grangertest(precio_cobre_diff~tasa_desempleo_diff, order = 12)

##No hay causalidad

# H0: El pib manufacturero no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El pib manufacturero sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 1)
grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 2)
grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 3)
grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 4)
grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 5)
grangertest(pib_manufacturas_diff~precio_cobre_diff, order = 6)
## 0.01108 *

###Al reves

grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 1)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 2)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 3)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 4)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 5)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 6)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 7)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 8)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 9)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 10)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 11)
grangertest(precio_cobre_diff~pib_manufacturas_diff, order = 12)

# H0: El pib de servicios no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El pib de servicios sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(pib_servicios_diff~precio_cobre_diff, order = 1)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 2)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 3)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 4)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 5)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 6)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 7)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 8)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 9)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 10)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 11)
grangertest(pib_servicios_diff~precio_cobre_diff, order = 12)

#No hay causalidad
##Al reves

grangertest(precio_cobre_diff~pib_servicios_diff, order = 1)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 2)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 3)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 4)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 5)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 6)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 7)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 8)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 9)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 10)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 11)
grangertest(precio_cobre_diff~pib_servicios_diff, order = 12)


# H0: El pib de minería no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El pib de minería sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(pib_minería_diff~precio_cobre_diff, order = 1)
grangertest(pib_minería_diff~precio_cobre_diff, order = 2)
grangertest(pib_minería_diff~precio_cobre_diff, order = 3)
grangertest(pib_minería_diff~precio_cobre_diff, order = 4)
#0.00001984 ***

##Al reves

grangertest(precio_cobre_diff~pib_minería_diff, order = 1)
##3.638e-11 ***

# H0: El VA de manufacturas no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El VA de manufacturas sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 1)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 2)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 3)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 4)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 5)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 6)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 7)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 8)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 9)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 10)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 11)
grangertest(VA_manufacturas_diff~precio_cobre_diff, order = 12)

##Al reves

grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 1)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 2)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 3)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 4)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 5)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 6)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 7)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 8)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 9)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 10)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 11)
grangertest(precio_cobre_diff~VA_manufacturas_diff, order = 12)

####Es un poco raro que no haya causalidad, crearemos otra deferencia para 
#precio del cobre y compararemos con la segunda diferencia de VA manufacturas

precio_cobre_diff2=diff(precio_cobre_diff)
adf.test(precio_cobre_diff2, alternative = "stationary")
#p-value = 0.01

grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 1)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 2)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 3)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 4)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 5)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 6)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 7)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 8)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 9)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 10)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 11)
grangertest(VA_manufacturas_diff2~precio_cobre_diff2, order = 12)


##Al reves

grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 1)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 2)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 3)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 4)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 5)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 6)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 7)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 8)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 9)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 10)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 11)
grangertest(precio_cobre_diff2~VA_manufacturas_diff2, order = 12)

##No hay causalidad

# H0: El VA de manufacturas no es causado en el sentido de granger por el VA de minería > 0,05
# H1: El VA de manufacturas sí es causado en el sentido de granger por el VA de minería < 0,05

grangertest(VA_manufacturas_diff~VA_minería_diff, order = 1)
##0.001994 **

#Al reves

grangertest(VA_minería_diff~VA_manufacturas_diff, order = 1)
# 0.005412 **

##Si bien no se encontró causalidad desde el precio del cobre hacia el 
#VA manufacturas, si se encontró causalidad desde VA minero a VA manufacturas



# H0: El VA de manufacturas no es causado en el sentido de granger por el Dolar Observado > 0,05
# H1: El VA de manufacturas sí es causado en el sentido de granger por el Dolar Observado < 0,05

grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 1)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 2)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 3)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 4)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 5)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 6)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 7)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 8)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 9)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 10)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 11)
grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 12)

##Al reves 

grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 1)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 2)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 3)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 4)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 5)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 6)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 7)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 8)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 9)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 10)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 11)
grangertest(tcambio_dolarobs_diff~VA_manufacturas_diff, order = 12)

tcambio_dolarobs_diff2=diff(tcambio_dolarobs_diff)
adf.test(tcambio_dolarobs_diff2, alternative = "stationary")
#p-value = 0.01

grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 1)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 2)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 3)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 4)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 5)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 6)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 7)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 8)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 9)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 10)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 11)
grangertest(VA_manufacturas_diff2~tcambio_dolarobs_diff2, order = 12)

#Al reves

grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 1)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 2)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 3)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 4)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 5)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 6)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 7)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 8)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 9)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 10)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 11)
grangertest(tcambio_dolarobs_diff2~VA_manufacturas_diff2, order = 12)


# H0: El VA de servicios no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El VA de servicios sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(VA_servicios_diff~precio_cobre_diff, order = 1)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 2)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 3)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 4)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 5)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 6)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 7)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 8)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 9)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 10)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 11)
grangertest(VA_servicios_diff~precio_cobre_diff, order = 12)

##Al reves
grangertest(precio_cobre_diff~VA_servicios_diff, order = 1)
##0.003794 **

# H0: El VA de Minería no es causado en el sentido de granger por el precio del cobre > 0,05
# H1: El VA de Minería sí es causado en el sentido de granger por el precio del cobre < 0,05

grangertest(VA_minería_diff~precio_cobre_diff, order = 1)
grangertest(VA_minería_diff~precio_cobre_diff, order = 2)
grangertest(VA_minería_diff~precio_cobre_diff, order = 3)
grangertest(VA_minería_diff~precio_cobre_diff, order = 4)
## 1.104e-05 ***

grangertest(precio_cobre_diff~VA_minería_diff, order = 1)
#5.525e-13 ***

###PASAMOS AL MODELO VAR
tcambio_real_diff=ts(tcambio_real_diff, start = 1985, freq = 4)
precio_cobre_diff=ts(precio_cobre_diff, start = 1985, freq = 4)
TPM_diff=ts(TPM_diff, start = 1985, freq = 4)
IPC_diff=ts(IPC_diff, start = 1985, freq = 4)
tcambio_dolarobs_diff=ts(tcambio_dolarobs_diff, start = 1985, freq = 4)
tasa_captación_diff=ts(tasa_captación_diff, start = 1985, freq = 4)
tasa_colocación_diff=ts(tasa_colocación_diff, start = 1985, freq = 4)
IPC_acum_diff=ts(IPC_acum_diff, start = 1985, freq = 4)
tasa_desempleo_diff=ts(tasa_desempleo_diff, start = 1985, freq = 4)
pib_manufacturas_diff=ts(pib_manufacturas_diff, start = 1985, freq = 4)
pib_servicios_diff=ts(pib_servicios_diff, start = 1985, freq = 4)
pib_minería_diff=ts(pib_minería_diff, start = 1985, freq = 4)
VA_manufacturas_diff2=ts(VA_manufacturas_diff2, start = 1985, freq = 4)
VA_servicios_diff=ts(VA_servicios_diff, start = 1985, freq = 4)
VA_minería_diff=ts(VA_minería_diff, start = 1985, freq = 4)
tipo_gob_diff=ts(tipo_gob_diff, start = 1985, freq = 4)
crisis_ec_diff=ts(crisis_ec_diff, start = 1985, freq = 4)
  
  
ejvar <- cbind(tcambio_dolarobs_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, pib_manufacturas_diff, pib_servicios_diff, pib_minería_diff, precio_cobre_diff, tipo_gob_diff, crisis_ec_diff)
ejvar

VARselect(ejvar, lag.max = 12)

#Los criterios indican:
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#12      12      1     12   

##Al utilizar 12 lags el VAR es tan grande que no cabe en la consola
#Este será el VAR A, y en el orden 1, por lo tanto, var_A1

var_A1 <- VAR(ejvar, p=1)
var_A1

summary(var_A1)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_A1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A1 <- serial.test(var_A1, lags.pt = 1, type = "PT.asymptotic")
serialvar_A1$serial

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A1=normality.test(var_A1)
normvar_A1$jb.mul

#Sesgo -> p-value = 2.2e-16
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A1 <- arch.test(var_A1, lags.multi = 1)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A1$arch.mul
#p-value = p-value = 3.331e-16, esto quiere decir que la varianza de los residuales no es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A1=irf(var_A1, response ="tcambio_dolarobs_diff", n.ahead=8, boot=TRUE)
irf_dolar_A1

plot(irf_dolar_A1)

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta de la TPM


irf_TPM_A1=irf(var_A1, response ="TPM_diff", n.ahead=8, boot=TRUE)
irf_TPM_A1

plot(irf_TPM_A1)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_A1=irf(var_A1, response ="IPC_diff", n.ahead=8, boot=TRUE)
irf_IPC_A1
plot(irf_IPC_A1)

#Ahora el PIB de manufacturas

irf_pib_manu_A1=irf(var_A1, response ="pib_manufacturas_diff", n.ahead=8, boot=TRUE)
irf_pib_manu_A1
plot(irf_pib_manu_A1)

#####PIB de servicios

irf_pib_serv_A1=irf(var_A1, response ="pib_servicios_diff", n.ahead=8, boot=TRUE)
irf_pib_serv_A1
plot(irf_pib_serv_A1)

#####PIB minería 


irf_pib_min_A1=irf(var_A1, response ="pib_minería_diff", n.ahead=8, boot=TRUE)
irf_pib_min_A1
plot(irf_pib_min_A1)

####Tasa de desempleo

irf_tasa_des_A1=irf(var_A1, response ="tasa_desempleo_diff", n.ahead=8, boot=TRUE)
irf_tasa_des_A1
plot(irf_tasa_des_A1)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_A1=irf(var_A1, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_pcob_A1

plot(irf_pcob_A1)



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

var_A2 <- VAR(ejvar, p=2)
var_A2

summary(var_A2)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_A2)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A2 <- serial.test(var_A2, lags.pt = 2, type = "PT.asymptotic")
serialvar_A2$serial

## 2.2e-16 Hay correlacion en los residuales

###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A2=normality.test(var_A2)
normvar_A2$jb.mul

#Sesgo -> p-value = 3.331e-16
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A2 <- arch.test(var_A2, lags.multi = 2)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A2$arch.mul
#p-value = 0.0006612, esto quiere decir que la varianza de los residuales no es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A2=irf(var_A2, response ="tcambio_dolarobs_diff", n.ahead=8, boot=TRUE)
irf_dolar_A2

plot(irf_dolar_A1)

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta de la TPM


irf_TPM_A2=irf(var_A2, response ="TPM_diff", n.ahead=8, boot=TRUE)
irf_TPM_A2

plot(irf_TPM_A2)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_A2=irf(var_A2, response ="IPC_diff", n.ahead=8, boot=TRUE)
irf_IPC_A2
plot(irf_IPC_A1)

#Ahora el PIB de manufacturas

irf_pib_manu_A2=irf(var_A2, response ="pib_manufacturas_diff", n.ahead=8, boot=TRUE)
irf_pib_manu_A2
plot(irf_pib_manu_A2)

#####PIB de servicios

irf_pib_serv_A2=irf(var_A2, response ="pib_servicios_diff", n.ahead=8, boot=TRUE)
irf_pib_serv_A2
plot(irf_pib_serv_A2)

#####PIB minería 


irf_pib_min_A2=irf(var_A2, response ="pib_minería_diff", n.ahead=8, boot=TRUE)
irf_pib_min_A2
plot(irf_pib_min_A2)

####Tasa de desempleo

irf_tasa_des_A2=irf(var_A2, response ="tasa_desempleo_diff", n.ahead=8, boot=TRUE)
irf_tasa_des_A2
plot(irf_tasa_des_A2)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_A2=irf(var_A2, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_pcob_A2

plot(irf_pcob_A2)





############################

var_A3 <- VAR(ejvar, p=3)
var_A3

summary(var_A3)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_A3)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A3 <- serial.test(var_A3, lags.pt = 3, type = "PT.asymptotic")
serialvar_A3$serial

## p-value < 2.2e-16

#Existe correlación en los residuales

###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A3=normality.test(var_A3)
normvar_A3$jb.mul

#Sesgo -> p-value = 3.302e-11
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A3 <- arch.test(var_A3, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A3$arch.mul
#p-value = 1 

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A3=irf(var_A3, response ="tcambio_dolarobs_diff", n.ahead=8, boot=TRUE)
irf_dolar_A3

plot(irf_dolar_A3)

##Este modelo impulso respuesta nos muestra como responde el valor del dolar ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

#Ahora el impulso respuesta de la TPM


irf_TPM_A3=irf(var_A3, response ="TPM_diff", n.ahead=8, boot=TRUE)
irf_TPM_A3

plot(irf_TPM_A3)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_A3=irf(var_A3, response ="IPC_diff", n.ahead=8, boot=TRUE)
irf_IPC_A3
plot(irf_IPC_A3)

#Ahora el PIB de manufacturas

irf_pib_manu_A3=irf(var_A3, response ="pib_manufacturas_diff", n.ahead=8, boot=TRUE)
irf_pib_manu_A3
plot(irf_pib_manu_A3)

#####PIB de servicios

irf_pib_serv_A3=irf(var_A3, response ="pib_servicios_diff", n.ahead=8, boot=TRUE)
irf_pib_serv_A3
plot(irf_pib_serv_A3)

#####PIB minería 


irf_pib_min_A3=irf(var_A3, response ="pib_minería_diff", n.ahead=8, boot=TRUE)
irf_pib_min_A3
plot(irf_pib_min_A3)

####Tasa de desempleo

irf_tasa_des_A3=irf(var_A3, response ="tasa_desempleo_diff", n.ahead=8, boot=TRUE)
irf_tasa_des_A3
plot(irf_tasa_des_A3)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_A3=irf(var_A3, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_pcob_A3

plot(irf_pcob_A3)

#####

#VARB -> Utiliza valor agregado sectorial en vez de pib 

ejvar2 <- cbind(tcambio_dolarobs_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, VA_manufacturas_diff2, VA_servicios_diff, VA_minería_diff, precio_cobre_diff, tipo_gob_diff, crisis_ec_diff)
ejvar2


VARselect(ejvar2, lag.max = 12)


ejvar$lag3_pib_servicios<- Lag(ejvar$pib_servicios_diff, +3)
ejvar

ejvar<-data.frame(ejvar)

grangertest(VA_manufacturas_diff~tcambio_dolarobs_diff, order = 1)


  
#Los criterios indican:
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#12      12      1     12   

##Al utilizar 12 lags el VAR es tan grande que no cabe en la consola
#Este será el VAR A, y en el orden 1, por lo tanto, var_A1

var_A1 <- VAR(ejvar, p=1)
var_A1

summary(var_A1)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_A1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A1 <- serial.test(var_A1, lags.pt = 1, type = "PT.asymptotic")
serialvar_A1$serial

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A1=normality.test(var_A1)
normvar_A1$jb.mul

#Sesgo -> p-value = 2.2e-16
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A1 <- arch.test(var_A1, lags.multi = 1)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A1$arch.mul
#p-value = p-value = 3.331e-16, esto quiere decir que la varianza de los residuales no es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A1=irf(var_A1, response ="tcambio_dolarobs_diff", n.ahead=8, boot=TRUE)
irf_dolar_A1

plot(irf_dolar_A1)

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta de la TPM


irf_TPM_A1=irf(var_A1, response ="TPM_diff", n.ahead=8, boot=TRUE)
irf_TPM_A1

plot(irf_TPM_A1)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_A1=irf(var_A1, response ="IPC_diff", n.ahead=8, boot=TRUE)
irf_IPC_A1
plot(irf_IPC_A1)

#Ahora el PIB de manufacturas

irf_pib_manu_A1=irf(var_A1, response ="pib_manufacturas_diff", n.ahead=8, boot=TRUE)
irf_pib_manu_A1
plot(irf_pib_manu_A1)

#####PIB de servicios

irf_pib_serv_A1=irf(var_A1, response ="pib_servicios_diff", n.ahead=8, boot=TRUE)
irf_pib_serv_A1
plot(irf_pib_serv_A1)

#####PIB minería 


irf_pib_min_A1=irf(var_A1, response ="pib_minería_diff", n.ahead=8, boot=TRUE)
irf_pib_min_A1
plot(irf_pib_min_A1)

####Tasa de desempleo

irf_tasa_des_A1=irf(var_A1, response ="tasa_desempleo_diff", n.ahead=8, boot=TRUE)
irf_tasa_des_A1
plot(irf_tasa_des_A1)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_A1=irf(var_A1, response ="precio_cobre_diff", n.ahead=8, boot=TRUE)
irf_pcob_A1

plot(irf_pcob_A1)


# VARSELECT indica orden 5
 


############ EL MODELO VAR (VAR REDUCIDO) NO TIENE EN CUENTA LA DEPENDENCIA ACTUAL, 
# YA QUE TODAS SUS VARIABLES DEPENDEN DE SUS PROPIOS VALORES PASADOS Y DE 
# LOS VALORES PASADOS DE LAS DEMÁS VARIABLES

#### LA DEPENDENCIA ACTUAL ES UN COMPONENTE QUE SE AÑADE, EN LOS MODELOS SVAR
## VECTORES AUTOREGRESIVOS ESTRUCTURALES (SVAR) ###
# SVAR

