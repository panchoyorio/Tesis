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
plot(cbind(IPC, IPC_acum), main="IPC - IPC var anual", col="blue")
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


#Prueba de Dickey Fuller

adf.test(tcambio_real, alternative = "stationary")
#p-value = 0.6533
adf.test(tcambio_dolarobs, alternative = "stationary")
#p-value = 0.5356
adf.test(TPM, alternative = "stationary")
#p-value =  0.04617
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

ndiffs(tcambio_real_diff)
ndiffs(tcambio_dolarobs_diff)
ndiffs(TPM_diff)
ndiffs(tasa_captación_diff)
ndiffs(tasa_colocación_diff)
ndiffs(IPC_diff)
ndiffs(IPC_acum_diff)
ndiffs(tasa_desempleo_diff)
ndiffs(precio_cobre_diff)
ndiffs(pib_manufacturas_diff)
ndiffs(pib_servicios_diff)
ndiffs(pib_minería_diff)
ndiffs(VA_manufacturas_diff)
ndiffs(VA_servicios_diff)
ndiffs(VA_minería_diff)
ndiffs(tipo_gob_diff)
ndiffs(crisis_ec_diff)

pib_servicios_diff2=diff(pib_servicios_diff)

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
adf.test(pib_servicios_diff2, alternative = "stationary")
#p-value = 0.01
adf.test(pib_minería_diff, alternative = "stationary")
#p-value =  0.01
adf.test(VA_manufacturas_diff, alternative = "stationary")
#p-value = 0.0964
adf.test(VA_servicios_diff, alternative = "stationary")
#p-value = 0.01
adf.test(VA_minería_diff, alternative = "stationary")
#p-value = 0.01
adf.test(tipo_gob_diff, alternative = "stationary")
#p-value = 0.01
adf.test(crisis_ec_diff, alternative = "stationary")
#p-value = 0.01


VA_manufacturas_diff2=diff(VA_manufacturas_diff)

## IRREGULARIDADES

ndiffs(pib_servicios_diff)
#[1] 1
ndiffs(VA_manufacturas_diff)
#[1] 0
adf.test(pib_servicios_diff, alternative = "stationary")
#p-value = 0.01
adf.test(VA_manufacturas_diff, alternative = "stationary")
#p-value = 0.0964


###PROBLEMAS CON ESTACIONARIEDAD_ EL TEST NDIFF SUGIERE CALCULAR PRIMERA DIF
#PARA TODAS LAS VARIABLES, EXEPTO LA CRISIS ECONOMICA (0) Y PIB SERVICIOS (2)

##POR OTRO LADO EL TEST DICKEY FULLER DICE QUE AMBAS VARIABLES (CRISIS Y PIB SERV)
#NECESITAN 1 SOLA DIFERENCIA, MIENTRAS QUE ES VALOR AÑADIDO EN MANUFACTURAS 
#LA QUE NECESITARÍA 2 DIF, Y TPM 0.

#Se hace uso de un analisis gráfico para determinar estacionariedad

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
plot(VA_manufacturas_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(VA_manufacturas_diff2, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(VA_servicios_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Servicios", col="blue")
plot(VA_minería_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Minero", col="blue")


#####IRREGULARES

plot(TPM, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(TPM_diff, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")
plot(pib_servicios_diff, ylab="PIB", xlab="Trimestres", main="Sector Servicios (1dif)", col="blue")
plot(pib_servicios_diff2, ylab="PIB", xlab="Trimestres", main="Sector Servicios (2dif)", col="blue")
plot(VA_manufacturas_diff, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(VA_manufacturas_diff2, ylab="Valor Añadido (% del PIB)", xlab="Trimestres", main="Sector Manufacturero", col="blue")
plot(crisis_ec, ylab="DUMMY", xlab="Trimestres", main="CRISIS", col="blue")
plot(crisis_ec_diff, ylab="DUMMY", xlab="Trimestres", main="CRISIS", col="blue")

par(mfrow=c(2,2), mar=c(4,4,4,1) + .1)

plot(TPM, ylab="TPM", xlab="Tiempo")
acf(TPM, main="Serie no Estacionaria (según ndiffs)")
plot(TPM_diff, ylab="TPM DIF", xlab="Tiempo")
acf(TPM_diff, main="Serie Estacionaria")

plot(pib_servicios_diff, ylab="PIB servicios dif", xlab="Tiempo")
acf(pib_servicios_diff, main="Serie no Estacionaria (según ndiffs)")
plot(pib_servicios_diff2, ylab="PIB servicios dif2", xlab="Tiempo")
acf(pib_servicios_diff2, main="Serie Estacionaria")

plot(VA_manufacturas_diff, ylab="VA manufacturas (% PIB) dif", xlab="Tiempo")
acf(VA_manufacturas_diff, main="Serie no Estacionaria (según dickey fuller)")
plot(VA_manufacturas_diff2, ylab="VA mnufacturas (% PIB) dif2", xlab="Tiempo")
acf(VA_manufacturas_diff2, main="Serie Estacionaria")

#En base a los graficos se decide calcular primera dif de TPM y PIB SERVICIOS
#y segunda dif de VA manufacturas


##Creo una base en data frame con las variables diferenciadas
##Elimino los NA



Base_datos2 <- cbind(tcambio_real_diff, tcambio_dolarobs_diff, IPC_diff, IPC_acum_diff, tasa_desempleo_diff, TPM_diff, tasa_captación_diff, tasa_colocación_diff, pib_manufacturas_diff, pib_servicios_diff, pib_minería_diff, VA_manufacturas_diff2, VA_servicios_diff, VA_minería_diff, precio_cobre_diff, tipo_gob_diff, crisis_ec_diff)

Base_datos2<-data.frame(Base_datos2)
Base_datos2 <- Base_datos2[-c(1),]

Base_datos2 = ts(Base_datos2, start = 1986, frequency = 4)

tcambio_real_diff = ts(Base_datos2[,1], start=1986, freq=4)
tcambio_real_diff
tcambio_dolarobs_diff = ts(Base_datos2[,2], start=1986, freq=4)
tcambio_dolarobs_diff
TPM_diff = ts(Base_datos2[,6], start=1986, freq=4)
TPM_diff
tasa_captación_diff = ts(Base_datos2[,7], start=1986, freq=4)
tasa_captación_diff
tasa_colocación_diff = ts(Base_datos2[,8], start=1986, freq=4)
tasa_colocación_diff
IPC_diff = ts(Base_datos2[,3], start=1986, freq=4)
IPC_diff
IPC_acum_diff = ts(Base_datos2[,4], start=1986, freq=4)
IPC_acum_diff
tasa_desempleo_diff = ts(Base_datos2[,5], start=1986, freq=4)
tasa_desempleo_diff
precio_cobre_diff= ts(Base_datos2[,15], start=1986, freq=4)
precio_cobre_diff
pib_manufacturas_diff =  ts(Base_datos2[,9], start=1986, freq=4)
pib_manufacturas_diff
pib_servicios_diff =  ts(Base_datos2[,10], start=1986, freq=4)
pib_servicios_diff
pib_minería_diff =  ts(Base_datos2[,11], start=1986, freq=4)
pib_minería_diff
VA_manufacturas_diff2 =  ts(Base_datos2[,12], start=1986, freq=4)
VA_manufacturas_diff2
VA_servicios_diff =  ts(Base_datos2[,13], start=1986, freq=4)
VA_servicios_diff
VA_minería_diff =  ts(Base_datos2[,14], start=1986, freq=4)
VA_minería_diff
tipo_gob_diff =  ts(Base_datos2[,16], start=1986, freq=4)
tipo_gob_diff
crisis_ec_diff=  ts(Base_datos2[,17], start=1986,  freq=4)
crisis_ec


par(mfrow=c(1,1), mar=c(4,4,4,1) + .1)

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
acf(VA_manufacturas_diff2)
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

par(mfrow=c(2,1), mar=c(3,3,3,1) + .1)

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
par(mfrow=c(1,1), mar=c(3,3,3,1) + .1)





## Test de causalidad de granger 

## Para aplicar lags manualmente en granger se aplican en la variable indepentiente

Base_datos2.1 <- Base_datos2
Base_datos2.1=data.frame(Base_datos2.1)
Base_datos2.1$lag1precio_cobre1_diff<- Lag(Base_datos2.1$precio_cobre_diff, +1)
Base_datos2.1$lag1tcambio_dolarobs1_diff<- Lag(Base_datos2.1$tcambio_dolarobs_diff, +1)

Base_datos2.1 <- Base_datos2.1[-c(1),]
Base_datos2.1 = ts(Base_datos2.1, start = 1986, frequency = 4)

tcambio_real1_diff = ts(Base_datos2.1[,1], start=1986, freq=4)
tcambio_dolarobs1_diff = ts(Base_datos2.1[,2], start=1986, freq=4)
TPM_diff1 = ts(Base_datos2.1[,6], start=1986, freq=4)
tasa_captación1_diff = ts(Base_datos2.1[,7], start=1986, freq=4)
tasa_colocación1_diff = ts(Base_datos2.1[,8], start=1986, freq=4)
IPC1_diff = ts(Base_datos2.1[,3], start=1986, freq=4)
IPC1_acum_diff = ts(Base_datos2.1[,4], start=1986, freq=4)
tasa_desempleo1_diff = ts(Base_datos2.1[,5], start=1986, freq=4)
precio_cobre1_diff= ts(Base_datos2.1[,6], start=1986, freq=4)
pib_manufacturas1_diff =  ts(Base_datos2.1[,9], start=1986, freq=4)
pib_servicios1_diff =  ts(Base_datos2.1[,10], start=1986, freq=4)
pib_minería1_diff =  ts(Base_datos2.1[,11], start=1986, freq=4)
VA_manufacturas1_diff2 =  ts(Base_datos2.1[,12], start=1986, freq=4)
VA_servicios1_diff =  ts(Base_datos2.1[,13], start=1986, freq=4)
VA_minería1_diff =  ts(Base_datos2.1[,14], start=1986, freq=4)
tipo_gob1_diff =  ts(Base_datos2.1[,16], start=1986, freq=4)
crisis1_ec =  ts(Base_datos2.1[,17], start=1986,  freq=4)
lag1precio_cobre1_diff= ts(Base_datos2.1[,18], start=1986, freq=4)
lag1tcambio_dolarobs_diff= ts(Base_datos2.1[,19], start=1986, freq=4)

grangertest(IPC1_diff~lag1precio_cobre1_diff, order = 1)
#0.06738
grangertest(pib_servicios1_diff~lag1precio_cobre1_diff, order = 1)
#0.3764
grangertest(VA_manufacturas1_diff2~lag1precio_cobre1_diff, order = 1)
#0.7658
grangertest(VA_servicios1_diff~lag1precio_cobre1_diff, order = 1)
#0.7033
grangertest(VA_manufacturas1_diff2~lag1tcambio_dolarobs_diff, order = 1)
#0.6277


Base_datos2.2 <- Base_datos2
Base_datos2.2=data.frame(Base_datos2.2)
Base_datos2.2$lag2precio_cobre2_diff<- Lag(Base_datos2.2$precio_cobre_diff, +2)
Base_datos2.2$lag2tcambio_dolarobs2_diff<- Lag(Base_datos2.2$tcambio_dolarobs_diff, +2)

Base_datos2.2 <- Base_datos2.2[-c(1,2),]
Base_datos2.2 = ts(Base_datos2.2, start = 1986, frequency = 4)

tcambio_real2_diff = ts(Base_datos2.2[,1], start=1986, freq=4)
tcambio_dolarobs2_diff = ts(Base_datos2.2[,2], start=1986, freq=4)
TPM2_diff = ts(Base_datos2.2[,6], start=1986, freq=4)
tasa_captación2_diff = ts(Base_datos2.2[,7], start=1986, freq=4)
tasa_colocación2_diff = ts(Base_datos2.2[,8], start=1986, freq=4)
IPC2_diff = ts(Base_datos2.2[,3], start=1986, freq=4)
IPC2_acum_diff = ts(Base_datos2.2[,4], start=1986, freq=4)
tasa_desempleo2_diff = ts(Base_datos2.2[,5], start=1986, freq=4)
precio_cobre2_diff= ts(Base_datos2.2[,6], start=1986, freq=4)
pib_manufacturas2_diff =  ts(Base_datos2.2[,9], start=1986, freq=4)
pib_servicios2_diff =  ts(Base_datos2.2[,10], start=1986, freq=4)
pib_minería2_diff =  ts(Base_datos2.2[,11], start=1986, freq=4)
VA_manufacturas2_diff2 =  ts(Base_datos2.2[,12], start=1986, freq=4)
VA_servicios2_diff =  ts(Base_datos2.2[,13], start=1986, freq=4)
VA_minería2_diff =  ts(Base_datos2.2[,14], start=1986, freq=4)
tipo_gob2_diff =  ts(Base_datos2.2[,16], start=1986, freq=4)
crisis2_ec =  ts(Base_datos2.2[,17], start=1986,  freq=4)
lag2precio_cobre2_diff= ts(Base_datos2.2[,18], start=1986, freq=4)
lag2tcambio_dolarobs2_diff= ts(Base_datos2.2[,19], start=1986, freq=4)

grangertest(IPC2_diff~lag2precio_cobre2_diff, order = 1)
#0.768
grangertest(pib_servicios2_diff~lag2precio_cobre2_diff, order = 1)
#0.0493 *
grangertest(VA_manufacturas2_diff2~lag2precio_cobre2_diff, order = 1)
#0.6426
grangertest(VA_servicios2_diff~lag2precio_cobre2_diff, order = 1)
# 0.4264
grangertest(VA_manufacturas2_diff2~lag2tcambio_dolarobs2_diff, order = 1)
#0.6693


Base_datos2.3 <- Base_datos2
Base_datos2.3=data.frame(Base_datos2.3)
Base_datos2.3$lag3precio_cobre3_diff<- Lag(Base_datos2.3$precio_cobre_diff, +3)
Base_datos2.3$lag3tcambio_dolarobs3_diff<- Lag(Base_datos2.3$tcambio_dolarobs_diff, +3)

Base_datos2.3 <- Base_datos2.3[-c(1,2,3),]
Base_datos2.3 = ts(Base_datos2.3, start = 1986, frequency = 4)

tcambio_real3_diff = ts(Base_datos2.3[,1], start=1986, freq=4)
tcambio_dolarobs3_diff = ts(Base_datos2.3[,2], start=1986, freq=4)
TPM3_diff = ts(Base_datos2.3[,6], start=1986, freq=4)
tasa_captación3_diff = ts(Base_datos2.3[,7], start=1986, freq=4)
tasa_colocación3_diff = ts(Base_datos2.3[,8], start=1986, freq=4)
IPC3_diff = ts(Base_datos2.3[,3], start=1986, freq=4)
IPC3_acum_diff = ts(Base_datos2.3[,4], start=1986, freq=4)
tasa_desempleo3_diff = ts(Base_datos2.3[,5], start=1986, freq=4)
precio_cobre3_diff= ts(Base_datos2.3[,6], start=1986, freq=4)
pib_manufacturas3_diff =  ts(Base_datos2.3[,9], start=1986, freq=4)
pib_servicios3_diff =  ts(Base_datos2.3[,10], start=1986, freq=4)
pib_minería3_diff =  ts(Base_datos2.3[,11], start=1986, freq=4)
VA_manufacturas3_diff2 =  ts(Base_datos2.3[,12], start=1986, freq=4)
VA_servicios3_diff =  ts(Base_datos2.3[,13], start=1986, freq=4)
VA_minería3_diff =  ts(Base_datos2.3[,14], start=1986, freq=4)
tipo_gob3_diff =  ts(Base_datos2.3[,16], start=1986, freq=4)
crisis3_ec =  ts(Base_datos2.3[,17], start=1986,  freq=4)
lag3precio_cobre3_diff= ts(Base_datos2.3[,18], start=1986, freq=4)
lag3tcambio_dolarobs3_diff= ts(Base_datos2.3[,19], start=1986, freq=4)

grangertest(IPC3_diff~lag3precio_cobre3_diff, order = 1)
#0.005423 **
grangertest(VA_manufacturas3_diff2~lag3precio_cobre3_diff, order = 1)
#0.7605
grangertest(VA_servicios3_diff~lag3precio_cobre3_diff, order = 1)
# 0.8841
grangertest(VA_manufacturas3_diff2~lag3tcambio_dolarobs3_diff, order = 1)
#0.6504


Base_datos2.4 <- Base_datos2
Base_datos2.4=data.frame(Base_datos2.4)
Base_datos2.4$lag4precio_cobre4_diff<- Lag(Base_datos2.4$precio_cobre_diff, +4)
Base_datos2.4$lag4tcambio_dolarobs4_diff<- Lag(Base_datos2.4$tcambio_dolarobs_diff, +4)

Base_datos2.4 <- Base_datos2.4[-c(1,2,3,4),]
Base_datos2.4 = ts(Base_datos2.4, start = 1986, frequency = 4)

tcambio_real4_diff = ts(Base_datos2.4[,1], start=1986, freq=4)
tcambio_dolarobs4_diff = ts(Base_datos2.4[,2], start=1986, freq=4)
TPM4_diff = ts(Base_datos2.4[,6], start=1986, freq=4)
tasa_captación4_diff = ts(Base_datos2.4[,7], start=1986, freq=4)
tasa_colocación4_diff = ts(Base_datos2.4[,8], start=1986, freq=4)
IPC4_diff = ts(Base_datos2.4[,3], start=1986, freq=4)
IPC4_acum_diff = ts(Base_datos2.4[,4], start=1986, freq=4)
tasa_desempleo4_diff = ts(Base_datos2.4[,5], start=1986, freq=4)
precio_cobre4_diff= ts(Base_datos2.4[,6], start=1986, freq=4)
pib_manufacturas4_diff =  ts(Base_datos2.4[,9], start=1986, freq=4)
pib_servicios4_diff =  ts(Base_datos2.4[,10], start=1986, freq=4)
pib_minería4_diff =  ts(Base_datos2.4[,11], start=1986, freq=4)
VA_manufacturas4_diff2 =  ts(Base_datos2.4[,12], start=1986, freq=4)
VA_servicios4_diff =  ts(Base_datos2.4[,13], start=1986, freq=4)
VA_minería4_diff =  ts(Base_datos2.4[,14], start=1986, freq=4)
tipo_gob4_diff =  ts(Base_datos2.4[,16], start=1986, freq=4)
crisis4_ec =  ts(Base_datos2.4[,17], start=1986,  freq=4)
lag4precio_cobre4_diff= ts(Base_datos2.4[,18], start=1986, freq=4)
lag4tcambio_dolarobs4_diff= ts(Base_datos2.4[,19], start=1986, freq=4)

grangertest(VA_manufacturas4_diff2~lag4precio_cobre4_diff, order = 1)
#0.5398
grangertest(VA_servicios4_diff~lag4precio_cobre4_diff, order = 1)
#0.04473 *
grangertest(VA_manufacturas4_diff2~lag4tcambio_dolarobs4_diff, order = 1)
#0.5346



Base_datos2.5 <- Base_datos2
Base_datos2.5=data.frame(Base_datos2.5)
Base_datos2.5$lag5precio_cobre5_diff<- Lag(Base_datos2.5$precio_cobre_diff, +5)
Base_datos2.5$lag5tcambio_dolarobs5_diff<- Lag(Base_datos2.5$tcambio_dolarobs_diff, +5)

Base_datos2.5 <- Base_datos2.5[-c(1,2,3,4,5),]
Base_datos2.5 = ts(Base_datos2.5, start = 1986, frequency = 4)

tcambio_real5_diff = ts(Base_datos2.5[,1], start=1986, freq=4)
tcambio_dolarobs5_diff = ts(Base_datos2.5[,2], start=1986, freq=4)
TPM5_diff = ts(Base_datos2.5[,6], start=1986, freq=4)
tasa_captación5_diff = ts(Base_datos2.5[,7], start=1986, freq=4)
tasa_colocación5_diff = ts(Base_datos2.5[,8], start=1986, freq=4)
IPC5_diff = ts(Base_datos2.5[,3], start=1986, freq=4)
IPC5_acum_diff = ts(Base_datos2.5[,4], start=1986, freq=4)
tasa_desempleo5_diff = ts(Base_datos2.5[,5], start=1986, freq=4)
precio_cobre5_diff= ts(Base_datos2.5[,6], start=1986, freq=4)
pib_manufacturas5_diff =  ts(Base_datos2.5[,9], start=1986, freq=4)
pib_servicios5_diff =  ts(Base_datos2.5[,10], start=1986, freq=4)
pib_minería5_diff =  ts(Base_datos2.5[,11], start=1986, freq=4)
VA_manufacturas5_diff2 =  ts(Base_datos2.5[,12], start=1986, freq=4)
VA_servicios5_diff =  ts(Base_datos2.5[,13], start=1986, freq=4)
VA_minería5_diff =  ts(Base_datos2.5[,14], start=1986, freq=4)
tipo_gob5_diff =  ts(Base_datos2.5[,16], start=1986, freq=4)
crisis5_ec =  ts(Base_datos2.5[,17], start=1986,  freq=4)
lag5precio_cobre5_diff= ts(Base_datos2.5[,18], start=1986, freq=4)
lag5tcambio_dolarobs5_diff= ts(Base_datos2.5[,19], start=1986, freq=4)

grangertest(VA_manufacturas5_diff2~lag5precio_cobre5_diff, order = 1)
#0.04474 *
grangertest(VA_manufacturas5_diff2~lag5tcambio_dolarobs5_diff, order = 1)
#0.5494

Base_datos2.6 <- Base_datos2
Base_datos2.6=data.frame(Base_datos2.6)
Base_datos2.6$lag6precio_cobre6_diff<- Lag(Base_datos2.6$precio_cobre_diff, +6)
Base_datos2.6$lag6tcambio_dolarobs6_diff<- Lag(Base_datos2.6$tcambio_dolarobs_diff, +6)

Base_datos2.6 <- Base_datos2.6[-c(1,2,3,4,5,6),]
Base_datos2.6 = ts(Base_datos2.6, start = 1986, frequency = 4)

tcambio_real6_diff = ts(Base_datos2.6[,1], start=1986, freq=4)
tcambio_dolarobs6_diff = ts(Base_datos2.6[,2], start=1986, freq=4)
TPM6_diff = ts(Base_datos2.6[,6], start=1986, freq=4)
tasa_captación6_diff = ts(Base_datos2.6[,7], start=1986, freq=4)
tasa_colocación6_diff = ts(Base_datos2.6[,8], start=1986, freq=4)
IPC6_diff = ts(Base_datos2.6[,3], start=1986, freq=4)
IPC6_acum_diff = ts(Base_datos2.6[,4], start=1986, freq=4)
tasa_desempleo6_diff = ts(Base_datos2.6[,5], start=1986, freq=4)
precio_cobre6_diff= ts(Base_datos2.6[,6], start=1986, freq=4)
pib_manufacturas6_diff =  ts(Base_datos2.6[,9], start=1986, freq=4)
pib_servicios6_diff =  ts(Base_datos2.6[,10], start=1986, freq=4)
pib_minería6_diff =  ts(Base_datos2.6[,11], start=1986, freq=4)
VA_manufacturas6_diff2 =  ts(Base_datos2.6[,12], start=1986, freq=4)
VA_servicios6_diff =  ts(Base_datos2.6[,13], start=1986, freq=4)
VA_minería6_diff =  ts(Base_datos2.6[,14], start=1986, freq=4)
tipo_gob6_diff =  ts(Base_datos2.6[,16], start=1986, freq=4)
crisis6_ec =  ts(Base_datos2.6[,17], start=1986,  freq=4)
lag6precio_cobre6_diff= ts(Base_datos2.6[,18], start=1986, freq=4)
lag6tcambio_dolarobs6_diff= ts(Base_datos2.6[,19], start=1986, freq=4)

grangertest(VA_manufacturas6_diff2~lag6tcambio_dolarobs6_diff, order = 1)
#0.7486



Base_datos2.7 <- Base_datos2
Base_datos2.7=data.frame(Base_datos2.7)
Base_datos2.7$lag7precio_cobre7_diff<- Lag(Base_datos2.7$precio_cobre_diff, +7)
Base_datos2.7$lag7tcambio_dolarobs7_diff<- Lag(Base_datos2.7$tcambio_dolarobs_diff, +7)

Base_datos2.7 <- Base_datos2.7[-c(1,2,3,4,5,6,7),]
Base_datos2.7 = ts(Base_datos2.7, start = 1986, frequency = 4)

tcambio_real7_diff = ts(Base_datos2.7[,1], start=1986, freq=4)
tcambio_dolarobs7_diff = ts(Base_datos2.7[,2], start=1986, freq=4)
TPM7_diff = ts(Base_datos2.7[,6], start=1986, freq=4)
tasa_captación7_diff = ts(Base_datos2.7[,7], start=1986, freq=4)
tasa_colocación7_diff = ts(Base_datos2.7[,8], start=1986, freq=4)
IPC7_diff = ts(Base_datos2.7[,3], start=1986, freq=4)
IPC7_acum_diff = ts(Base_datos2.7[,4], start=1986, freq=4)
tasa_desempleo7_diff = ts(Base_datos2.7[,5], start=1986, freq=4)
precio_cobre7_diff= ts(Base_datos2.7[,6], start=1986, freq=4)
pib_manufacturas7_diff =  ts(Base_datos2.7[,9], start=1986, freq=4)
pib_servicios7_diff =  ts(Base_datos2.7[,10], start=1986, freq=4)
pib_minería7_diff =  ts(Base_datos2.7[,11], start=1986, freq=4)
VA_manufacturas7_diff2 =  ts(Base_datos2.7[,12], start=1986, freq=4)
VA_servicios7_diff =  ts(Base_datos2.7[,13], start=1986, freq=4)
VA_minería7_diff =  ts(Base_datos2.7[,14], start=1986, freq=4)
tipo_gob7_diff =  ts(Base_datos2.7[,16], start=1986, freq=4)
crisis7_ec =  ts(Base_datos2.7[,17], start=1986,  freq=4)
lag7precio_cobre7_diff= ts(Base_datos2.7[,18], start=1986, freq=4)
lag7tcambio_dolarobs7_diff= ts(Base_datos2.7[,19], start=1986, freq=4)

grangertest(VA_manufacturas7_diff2~lag7tcambio_dolarobs7_diff, order = 1)
#0.5519


Base_datos2.8 <- Base_datos2
Base_datos2.8=data.frame(Base_datos2.8)
Base_datos2.8$lag8precio_cobre8_diff<- Lag(Base_datos2.8$precio_cobre_diff, +8)
Base_datos2.8$lag8tcambio_dolarobs8_diff<- Lag(Base_datos2.8$tcambio_dolarobs_diff, +8)

Base_datos2.8 <- Base_datos2.8[-c(1,2,3,4,5,6,7,8),]
Base_datos2.8 = ts(Base_datos2.8, start = 1986, frequency = 4)

tcambio_real8_diff = ts(Base_datos2.8[,1], start=1986, freq=4)
tcambio_dolarobs8_diff = ts(Base_datos2.8[,2], start=1986, freq=4)
TPM8_diff = ts(Base_datos2.8[,6], start=1986, freq=4)
tasa_captación8_diff = ts(Base_datos2.8[,7], start=1986, freq=4)
tasa_colocación8_diff = ts(Base_datos2.8[,8], start=1986, freq=4)
IPC8_diff = ts(Base_datos2.8[,3], start=1986, freq=4)
IPC8_acum_diff = ts(Base_datos2.8[,4], start=1986, freq=4)
tasa_desempleo8_diff = ts(Base_datos2.8[,5], start=1986, freq=4)
precio_cobre8_diff= ts(Base_datos2.8[,6], start=1986, freq=4)
pib_manufacturas8_diff =  ts(Base_datos2.8[,9], start=1986, freq=4)
pib_servicios8_diff =  ts(Base_datos2.8[,10], start=1986, freq=4)
pib_minería8_diff =  ts(Base_datos2.8[,11], start=1986, freq=4)
VA_manufacturas8_diff2 =  ts(Base_datos2.8[,12], start=1986, freq=4)
VA_servicios8_diff =  ts(Base_datos2.8[,13], start=1986, freq=4)
VA_minería8_diff =  ts(Base_datos2.8[,14], start=1986, freq=4)
tipo_gob8_diff =  ts(Base_datos2.8[,16], start=1986, freq=4)
crisis8_ec =  ts(Base_datos2.8[,17], start=1986,  freq=4)
lag8precio_cobre8_diff= ts(Base_datos2.8[,18], start=1986, freq=4)
lag8tcambio_dolarobs8_diff= ts(Base_datos2.8[,19], start=1986, freq=4)

grangertest(VA_manufacturas8_diff2~lag8tcambio_dolarobs8_diff, order = 1)
#0.6852

Base_datos2.9 <- Base_datos2
Base_datos2.9=data.frame(Base_datos2.9)
Base_datos2.9$lag9precio_cobre9_diff<- Lag(Base_datos2.9$precio_cobre_diff, +9)
Base_datos2.9$lag9tcambio_dolarobs9_diff<- Lag(Base_datos2.9$tcambio_dolarobs_diff, +9)

Base_datos2.9 <- Base_datos2.9[-c(1,2,3,4,5,6,7,8,9),]
Base_datos2.9 = ts(Base_datos2.9, start = 1986, frequency = 4)

tcambio_real9_diff = ts(Base_datos2.9[,1], start=1986, freq=4)
tcambio_dolarobs9_diff = ts(Base_datos2.9[,2], start=1986, freq=4)
TPM9_diff = ts(Base_datos2.9[,6], start=1986, freq=4)
tasa_captación9_diff = ts(Base_datos2.9[,7], start=1986, freq=4)
tasa_colocación9_diff = ts(Base_datos2.9[,8], start=1986, freq=4)
IPC9_diff = ts(Base_datos2.9[,3], start=1986, freq=4)
IPC9_acum_diff = ts(Base_datos2.9[,4], start=1986, freq=4)
tasa_desempleo9_diff = ts(Base_datos2.9[,5], start=1986, freq=4)
precio_cobre9_diff= ts(Base_datos2.9[,6], start=1986, freq=4)
pib_manufacturas9_diff =  ts(Base_datos2.9[,9], start=1986, freq=4)
pib_servicios9_diff =  ts(Base_datos2.9[,10], start=1986, freq=4)
pib_minería9_diff =  ts(Base_datos2.9[,11], start=1986, freq=4)
VA_manufacturas9_diff2 =  ts(Base_datos2.9[,12], start=1986, freq=4)
VA_servicios9_diff =  ts(Base_datos2.9[,13], start=1986, freq=4)
VA_minería9_diff =  ts(Base_datos2.9[,14], start=1986, freq=4)
tipo_gob9_diff =  ts(Base_datos2.9[,16], start=1986, freq=4)
crisis9_ec =  ts(Base_datos2.9[,17], start=1986,  freq=4)
lag9precio_cobre9_diff= ts(Base_datos2.9[,18], start=1986, freq=4)
lag9tcambio_dolarobs9_diff= ts(Base_datos2.9[,19], start=1986, freq=4)

grangertest(VA_manufacturas9_diff2~lag9tcambio_dolarobs9_diff, order = 1)
#0.6917

Base_datos2.10 <- Base_datos2
Base_datos2.10=data.frame(Base_datos2.10)
Base_datos2.10$lag10precio_cobre10_diff<- Lag(Base_datos2.10$precio_cobre_diff, +10)
Base_datos2.10$lag10tcambio_dolarobs10_diff<- Lag(Base_datos2.10$tcambio_dolarobs_diff, +10)

Base_datos2.10 <- Base_datos2.10[-c(1,2,3,4,5,6,7,8,9,10),]
Base_datos2.10 = ts(Base_datos2.10, start = 1986, frequency = 4)

tcambio_real10_diff = ts(Base_datos2.10[,1], start=1986, freq=4)
tcambio_dolarobs10_diff = ts(Base_datos2.10[,2], start=1986, freq=4)
TPM10_diff = ts(Base_datos2.10[,6], start=1986, freq=4)
tasa_captación10_diff = ts(Base_datos2.10[,7], start=1986, freq=4)
tasa_colocación10_diff = ts(Base_datos2.10[,8], start=1986, freq=4)
IPC10_diff = ts(Base_datos2.10[,3], start=1986, freq=4)
IPC10_acum_diff = ts(Base_datos2.10[,4], start=1986, freq=4)
tasa_desempleo10_diff = ts(Base_datos2.10[,5], start=1986, freq=4)
precio_cobre10_diff= ts(Base_datos2.10[,6], start=1986, freq=4)
pib_manufacturas10_diff =  ts(Base_datos2.10[,9], start=1986, freq=4)
pib_servicios10_diff =  ts(Base_datos2.10[,10], start=1986, freq=4)
pib_minería10_diff =  ts(Base_datos2.10[,11], start=1986, freq=4)
VA_manufacturas10_diff2 =  ts(Base_datos2.10[,12], start=1986, freq=4)
VA_servicios10_diff =  ts(Base_datos2.10[,13], start=1986, freq=4)
VA_minería10_diff =  ts(Base_datos2.10[,14], start=1986, freq=4)
tipo_gob10_diff =  ts(Base_datos2.10[,16], start=1986, freq=4)
crisis10_ec =  ts(Base_datos2.10[,17], start=1986,  freq=4)
lag10precio_cobre10_diff= ts(Base_datos2.10[,18], start=1986, freq=4)
lag10tcambio_dolarobs10_diff= ts(Base_datos2.10[,19], start=1986, freq=4)

grangertest(VA_manufacturas10_diff2~lag10tcambio_dolarobs10_diff, order = 1)
#0.5956

Base_datos2.11 <- Base_datos2
Base_datos2.11=data.frame(Base_datos2.11)
Base_datos2.11$lag11precio_cobre11_diff<- Lag(Base_datos2.11$precio_cobre_diff, +11)
Base_datos2.11$lag11tcambio_dolarobs11_diff<- Lag(Base_datos2.11$tcambio_dolarobs_diff, +11)

Base_datos2.11 <- Base_datos2.11[-c(1,2,3,4,5,6,7,8,9,10,11),]
Base_datos2.11 = ts(Base_datos2.11, start = 1986, frequency = 4)

tcambio_real11_diff = ts(Base_datos2.11[,1], start=1986, freq=4)
tcambio_dolarobs11_diff = ts(Base_datos2.11[,2], start=1986, freq=4)
TPM11_diff = ts(Base_datos2.11[,6], start=1986, freq=4)
tasa_captación11_diff = ts(Base_datos2.11[,7], start=1986, freq=4)
tasa_colocación11_diff = ts(Base_datos2.11[,8], start=1986, freq=4)
IPC11_diff = ts(Base_datos2.11[,3], start=1986, freq=4)
IPC11_acum_diff = ts(Base_datos2.11[,4], start=1986, freq=4)
tasa_desempleo11_diff = ts(Base_datos2.11[,5], start=1986, freq=4)
precio_cobre11_diff= ts(Base_datos2.11[,6], start=1986, freq=4)
pib_manufacturas11_diff =  ts(Base_datos2.11[,9], start=1986, freq=4)
pib_servicios11_diff =  ts(Base_datos2.11[,10], start=1986, freq=4)
pib_minería11_diff =  ts(Base_datos2.11[,11], start=1986, freq=4)
VA_manufacturas11_diff2 =  ts(Base_datos2.11[,12], start=1986, freq=4)
VA_servicios11_diff =  ts(Base_datos2.11[,13], start=1986, freq=4)
VA_minería11_diff =  ts(Base_datos2.11[,14], start=1986, freq=4)
tipo_gob11_diff =  ts(Base_datos2.11[,16], start=1986, freq=4)
crisis11_ec =  ts(Base_datos2.11[,17], start=1986,  freq=4)
lag11precio_cobre11_diff= ts(Base_datos2.11[,18], start=1986, freq=4)
lag11tcambio_dolarobs11_diff= ts(Base_datos2.11[,19], start=1986, freq=4)

grangertest(VA_manufacturas11_diff2~lag11tcambio_dolarobs11_diff, order = 1)
#0.1179

Base_datos2.12 <- Base_datos2
Base_datos2.12=data.frame(Base_datos2.12)
Base_datos2.12$lag12precio_cobre12_diff<- Lag(Base_datos2.12$precio_cobre_diff, +12)
Base_datos2.12$lag12tcambio_dolarobs12_diff<- Lag(Base_datos2.12$tcambio_dolarobs_diff, +12)

Base_datos2.12 <- Base_datos2.12[-c(1,2,3,4,5,6,7,8,9,10,11,12),]
Base_datos2.12 = ts(Base_datos2.12, start = 1986, frequency = 4)

tcambio_real12_diff = ts(Base_datos2.12[,1], start=1986, freq=4)
tcambio_dolarobs12_diff = ts(Base_datos2.12[,2], start=1986, freq=4)
TPM12_diff = ts(Base_datos2.12[,6], start=1986, freq=4)
tasa_captación12_diff = ts(Base_datos2.12[,7], start=1986, freq=4)
tasa_colocación12_diff = ts(Base_datos2.12[,8], start=1986, freq=4)
IPC12_diff = ts(Base_datos2.12[,3], start=1986, freq=4)
IPC12_acum_diff = ts(Base_datos2.12[,4], start=1986, freq=4)
tasa_desempleo12_diff = ts(Base_datos2.12[,5], start=1986, freq=4)
precio_cobre12_diff= ts(Base_datos2.12[,6], start=1986, freq=4)
pib_manufacturas12_diff =  ts(Base_datos2.12[,9], start=1986, freq=4)
pib_servicios12_diff =  ts(Base_datos2.12[,10], start=1986, freq=4)
pib_minería12_diff =  ts(Base_datos2.12[,11], start=1986, freq=4)
VA_manufacturas12_diff2 =  ts(Base_datos2.12[,12], start=1986, freq=4)
VA_servicios12_diff =  ts(Base_datos2.12[,13], start=1986, freq=4)
VA_minería12_diff =  ts(Base_datos2.12[,14], start=1986, freq=4)
tipo_gob12_diff =  ts(Base_datos2.12[,16], start=1986, freq=4)
crisis12_ec =  ts(Base_datos2.12[,17], start=1986,  freq=4)
lag12precio_cobre12_diff= ts(Base_datos2.12[,18], start=1986, freq=4)
lag12tcambio_dolarobs12_diff= ts(Base_datos2.12[,19], start=1986, freq=4)

grangertest(VA_manufacturas12_diff2~lag12tcambio_dolarobs12_diff, order = 1)
#0.2244





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
tcambio_real_diff=ts(tcambio_real_diff, start = 1986, freq = 4)
precio_cobre_diff=ts(precio_cobre_diff, start = 1986, freq = 4)
TPM_diff=ts(TPM_diff, start = 1986, freq = 4)
IPC_diff=ts(IPC_diff, start = 1986, freq = 4)
tcambio_dolarobs_diff=ts(tcambio_dolarobs_diff, start = 1986, freq = 4)
tasa_captación_diff=ts(tasa_captación_diff, start = 1986, freq = 4)
tasa_colocación_diff=ts(tasa_colocación_diff, start = 1986, freq = 4)
IPC_acum_diff=ts(IPC_acum_diff, start = 1986, freq = 4)
tasa_desempleo_diff=ts(tasa_desempleo_diff, start = 1986, freq = 4)
pib_manufacturas_diff=ts(pib_manufacturas_diff, start = 1986, freq = 4)
pib_servicios_diff=ts(pib_servicios_diff, start = 1986, freq = 4)
pib_minería_diff=ts(pib_minería_diff, start = 1986, freq = 4)
VA_manufacturas_diff2=ts(VA_manufacturas_diff2, start = 1986, freq = 4)
VA_servicios_diff=ts(VA_servicios_diff, start = 1986, freq = 4)
VA_minería_diff=ts(VA_minería_diff, start = 1986, freq = 4)
tipo_gob_diff=ts(tipo_gob_diff, start = 1986, freq = 4)
crisis_ec_diff=ts(crisis_ec_diff, start = 1986, freq = 4)
  
  
ejvarA1 <- cbind(precio_cobre_diff, tcambio_dolarobs_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, pib_manufacturas_diff, pib_servicios_diff, pib_minería_diff, tipo_gob_diff, crisis_ec_diff)
ejvarA1
#ejvar <- ejvar[-c(139),]

VARselect(ejvarA1, lag.max = 3)

#Los criterios indican:
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      1      1      3  

##Al utilizar 12 lags el VAR es tan grande que no cabe en la consola
#Este será el VAR A, y en el orden 1, por lo tanto, var_A1

var_A1 <- VAR(ejvarA1, p=3)
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

serialvar_A1 <- serial.test(var_A1, lags.pt = 3, type = "PT.asymptotic")
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

#Sesgo -> p-value = 7.013e-13
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A1 <- arch.test(var_A1, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A1$arch.mul
#p-value =  1, esto quiere decir que la varianza de los residuales es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="tcambio_dolarobs_diff", n.ahead=12, boot=TRUE)
irf_dolar_A1

plot(irf_dolar_A1, main="Función Impulso Respuesta, Precio del Cobre - Dolar Observado (VAR A1)")

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf


#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="IPC_diff", n.ahead=12, boot=TRUE)
irf_IPC_A1
plot(irf_IPC_A1, main="Función Impulso Respuesta, Precio del Cobre - IPC (VAR A1)")


#Ahora el impuldo respuesta de la TPM


irf_TPM_A1=irf(var_A1,impulse = "precio_cobre_diff", response ="TPM_diff", n.ahead=12, boot=TRUE)
irf_TPM_A1

plot(irf_TPM_A1, main="Función Impulso Respuesta, Precio del Cobre - TPM (VAR A1)")


#Ahora el PIB de manufacturas

irf_pib_manu_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="pib_manufacturas_diff", n.ahead=12, boot=TRUE)
irf_pib_manu_A1
plot(irf_pib_manu_A1, main="Función Impulso Respuesta, Precio del Cobre - PIB Manufacturas (VAR A1)")

#####PIB de servicios

irf_pib_serv_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="pib_servicios_diff", n.ahead=12, boot=TRUE)
irf_pib_serv_A1
plot(irf_pib_serv_A1, main="Función Impulso Respuesta, Precio del Cobre - PIB Servicios (VAR A1)")

#####PIB minería 


irf_pib_min_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="pib_minería_diff", n.ahead=12, boot=TRUE)
irf_pib_min_A1
plot(irf_pib_min_A1, main="Función Impulso Respuesta, Precio del Cobre - PIB Minería (VAR A1)")

####Tasa de desempleo

irf_tasa_des_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="tasa_desempleo_diff", n.ahead=12, boot=TRUE)
irf_tasa_des_A1
plot(irf_tasa_des_A1, main="Función Impulso Respuesta, Precio del Cobre - Tasa de Desempleo (VAR A1)")


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_A1=irf(var_A1, impulse = "precio_cobre_diff", response ="precio_cobre_diff", n.ahead=12, boot=TRUE)
irf_pcob_A1

plot(irf_pcob_A1, main="Función Impulso Respuesta, Precio del Cobre - Precio del Cobre (VAR A1)")



###Impulso respuesta acumulado

#irf_pcob_ac=irf(var1, response ="precio_cobre_diff", n.ahead=8, ortho = FALSE,
#             cumulative=TRUE)
#irf_pcob_ac
#plot(irf_pcob_ac)

#Descompocición de la varianza ante una innovación en el precio del cobre
#(le estamos pidiendo 50 observaciones hacia adelante como pronostico)
#DESVAR_pcob=fevd(var1, n.ahead=50)$precio_cobre_diff
#DESVAR_pcob

#Descompocición de la varianza ante una innovación en el tipo de cambio real
#DESVAR_tcamr=fevd(var1, n.ahead=50)$tcambio_real_diff
#DESVAR_tcamr
#plot(DESVAR_pcob)
#Descompocición de la varianza ante una innovación en la TPM
#DESVAR_TPM=fevd(var1, n.ahead=50)$tasa_TPM_diff
#DESVAR_TPM


#Descompocición de la varianza ante una innovación en el IPC
#DESVAR_IPC=fevd(var1, n.ahead=50)$tasa_IPC_diff
#DESVAR_IPC




############################


ejvarA2 <- cbind(precio_cobre_diff, tcambio_real_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, pib_manufacturas_diff, pib_servicios_diff, pib_minería_diff, tipo_gob_diff, crisis_ec_diff)
ejvarA2
#ejvar <- ejvar[-c(139),]

VARselect(ejvarA2, lag.max = 3)

#Los criterios indican:
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      1      1      3  

##Al utilizar 12 lags el VAR es tan grande que no cabe en la consola
#Este será el VAR A, y en el orden 1, por lo tanto, var_A1

var_A2 <- VAR(ejvarA2, p=3)
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

serialvar_A2 <- serial.test(var_A2, lags.pt = 3, type = "PT.asymptotic")
serialvar_A2$serial

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A2=normality.test(var_A2)
normvar_A2$jb.mul

#Sesgo -> p-value =  1.032e-09
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A2 <- arch.test(var_A2, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A2$arch.mul
#p-value =  1, esto quiere decir que la varianza de los residuales es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="tcambio_real_diff", n.ahead=12, boot=TRUE)
irf_dolar_A2

plot(irf_dolar_A2, main="Función Impulso Respuesta, Precio del Cobre - Tipo de Cambio Real (VAR A2)")

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta del IPC


irf_IPC_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="IPC_diff", n.ahead=12, boot=TRUE)
irf_IPC_A2

plot(irf_IPC_A2, main="Función Impulso Respuesta, Precio del Cobre - IPC (VAR A2)")

#Ahora el impulso respuesta de la TPM frente a innovaciones en las otras variables

irf_TPM_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="TPM_diff", n.ahead=12, boot=TRUE)
irf_TPM_A2

plot(irf_TPM_A2, main="Función Impulso Respuesta, Precio del Cobre - TPM (VAR A2)")

#Ahora el PIB de manufacturas

irf_pib_manu_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="pib_manufacturas_diff", n.ahead=12, boot=TRUE)
irf_pib_manu_A2
plot(irf_pib_manu_A2, main="Función Impulso Respuesta, Precio del Cobre - PIB Manufacturas (VAR A2)")

#####PIB de servicios


irf_pib_serv_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="pib_servicios_diff", n.ahead=12, boot=TRUE)
irf_pib_serv_A2
plot(irf_pib_serv_A2, main="Función Impulso Respuesta, Precio del Cobre - PIB Servicios (VAR A2)")


#####PIB minería 


irf_pib_min_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="pib_minería_diff", n.ahead=12, boot=TRUE)
irf_pib_min_A2
plot(irf_pib_min_A2, main="Función Impulso Respuesta, Precio del Cobre - PIB Minería (VAR A2)")

####Tasa de desempleo

irf_tasa_des_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="tasa_desempleo_diff", n.ahead=12, boot=TRUE)
irf_tasa_des_A2
plot(irf_tasa_des_A2, main="Función Impulso Respuesta, Precio del Cobre - Tasa de Desempleo (VAR A2)")

#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_precio_cobre_A2=irf(var_A2, impulse = "precio_cobre_diff", response ="precio_cobre_diff", n.ahead=12, boot=TRUE)
irf_precio_cobre_A2
plot(irf_precio_cobre_A2)







##############################################################

ejvarA3 <- cbind(precio_cobre_diff, tcambio_dolarobs_diff, IPC_acum_diff, tasa_desempleo_diff, TPM_diff, pib_manufacturas_diff, pib_servicios_diff, pib_minería_diff, tipo_gob_diff, crisis_ec_diff)
ejvarA3
#ejvar <- ejvar[-c(139),]

VARselect(ejvarA3, lag.max = 3)

#Los criterios indican:
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      1      1      3  

##Al utilizar 12 lags el VAR es tan grande que no cabe en la consola
#Este será el VAR A, y en el orden 1, por lo tanto, var_A1

var_A3 <- VAR(ejvarA3, p=3)
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

## en nuestro caso obtuvimos 2.2e-16, que es muy cercano a 0 y < a 0,05
#Por ende, rechazamos la hipotesis nula, los residuales sí están correlaciondos 
#Hay presencia de correlación serial


###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_A3=normality.test(var_A3)
normvar_A3$jb.mul

#Sesgo -> p-value = 1.831e-11
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_A3 <- arch.test(var_A3, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_A3$arch.mul
#p-value =  1, esto quiere decir que la varianza de los residuales es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="tcambio_dolarobs_diff", n.ahead=12, boot=TRUE)
irf_dolar_A3

plot(irf_dolar_A3, main="Función Impulso Respuesta, Precio del Cobre - Dolar Observado (VAR A3)")

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta de la TPM


irf_IPC_acum_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="IPC_acum_diff", n.ahead=12, boot=TRUE)
irf_IPC_acum_A3

plot(irf_IPC_acum_A3, main="Función Impulso Respuesta, Precio del Cobre - IPC Variación Anual (VAR A3)")

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables

irf_TPM_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="TPM_diff", n.ahead=12, boot=TRUE)
irf_TPM_A3

plot(irf_TPM_A3, main="Función Impulso Respuesta, Precio del Cobre - TPM (VAR A3)")

#Ahora el PIB de manufacturas

irf_pib_manu_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="pib_manufacturas_diff", n.ahead=12, boot=TRUE)
irf_pib_manu_A3
plot(irf_pib_manu_A3, main="Función Impulso Respuesta, Precio del Cobre - PIB Manufacturas (VAR A3)")

#####PIB de servicios


irf_pib_serv_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="pib_servicios_diff", n.ahead=12, boot=TRUE)
irf_pib_serv_A3
plot(irf_pib_serv_A3, main="Función Impulso Respuesta, Precio del Cobre - PIB Servicios (VAR A3)")


#####PIB minería 


irf_pib_min_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="pib_minería_diff", n.ahead=12, boot=TRUE)
irf_pib_min_A3
plot(irf_pib_min_A3, main="Función Impulso Respuesta, Precio del Cobre - PIB Minería (VAR A3)")

####Tasa de desempleo

irf_tasa_des_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="tasa_desempleo_diff", n.ahead=12, boot=TRUE)
irf_tasa_des_A3
plot(irf_tasa_des_A3, main="Función Impulso Respuesta, Precio del Cobre - Tasa de Desempleo (VAR A3)")

#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_precio_cobre_A3=irf(var_A3, impulse = "precio_cobre_diff", response ="precio_cobre_diff", n.ahead=12, boot=TRUE)
irf_precio_cobre_A3
plot(irf_precio_cobre_A3, main="Función Impulso Respuesta, Precio del Cobre - Precio del Cobre (VAR A3)")



###########################################################

ejvarB1- cbind(precio_cobre_diff, tcambio_dolarobs_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, VA_manufacturas_diff2, VA_servicios_diff, VA_minería_diff, tipo_gob_diff, crisis_ec_diff)
ejvarB1
VARselect(ejvarB1 ax = 4)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      1      1      3 

var_B1 <- VAR(ejvar2, p=3)
var_B1


summary(var_B1)
##Roots of the characteristic polynomial:
## < 1
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_B1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_B1 <- serial.test(var_B1, lags.pt = 3, type = "PT.asymptotic")
serialvar_B1$serial

## 2.2e-16 Hay correlacion en los residuales

###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_B1=normality.test(var_B1)
normvar_B1$jb.mul

#Sesgo -> p-value = 5.214e-06
#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_B1 <- arch.test(var_B1, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_B1$arch.mul
#p-value = 1, esto quiere decir que la varianza de los residuales si es constante
##REVISAR

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_B1=irf(var_B1, response ="tcambio_dolarobs_diff", n.ahead=12, boot=TRUE)
irf_dolar_B1

plot(irf_dolar_B1)

##Este modelo impulso respuesta nos muestra como responde el precio del cobre ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf




#Ahora el impuldo respuesta de la TPM


irf_TPM_B1=irf(var_B1, response ="TPM_diff", n.ahead=12, boot=TRUE)
irf_TPM_B1

plot(irf_TPM_B1)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_B1=irf(var_B1, response ="IPC_diff", n.ahead=12, boot=TRUE)
irf_IPC_B1
plot(irf_IPC_B1)

#Ahora el PIB de manufacturas

irf_va_manu_B1=irf(var_B1, response ="VA_manufacturas_diff2", n.ahead=12, boot=TRUE)
irf_va_manu_B1
plot(irf_va_manu_B1)

#####PIB de servicios

irf_va_serv_B1=irf(var_B1, response ="VA_servicios_diff", n.ahead=12, boot=TRUE)
irf_va_serv_B1
plot(irf_va_serv_B1)

#####PIB minería 


irf_va_min_B1=irf(var_B1, response ="VA_minería_diff", n.ahead=12, boot=TRUE)
irf_va_min_B1
plot(irf_pib_min_B1)

####Tasa de desempleo

irf_tasa_des_B1=irf(var_B1, response ="tasa_desempleo_diff", n.ahead=12, boot=TRUE, impulse = "precio_cobre_diff")
irf_tasa_des_B1

plot(irf_tasa_des_B1)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_pcob_B1=irf(var_B1, response ="precio_cobre_diff", n.ahead=12, boot=TRUE)
irf_pcob_B1

plot(irf_pcob_B1)



############################

ejvar3 <- cbind(precio_cobre_diff, tcambio_real_diff, IPC_diff, tasa_desempleo_diff, TPM_diff, VA_manufacturas_diff2, VA_servicios_diff, VA_minería_diff, tipo_gob_diff, crisis_ec_diff)
ejvar3
VARselect(ejvar3, lag.max = 3)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      1      1      3 


var_B2 <- VAR(ejvar3, p=3)
var_B2

summary(var_B2)
##Roots of the characteristic polynomial:
## 0.3095 0.2996 0.2996 0.2861
## Como son todas menores a 1, podemos decir que se satisface la condición 
#de estabilidad.

#Lo graficamos
plot(var_B2)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_B2 <- serial.test(var_B2, lags.pt = 3, type = "PT.asymptotic")
serialvar_B2$serial

## p-value < 2.2e-16

#Existe correlación en los residuales

###Procedemos a hacer la prueba de normaliad de los residuales

## Nos vamos a fijar en los p value de la kurtosis y del sesgo (skewness)

#H0: Los residuales se distribuyen normal   (pvalue > 0,05 -> Aceptamos H0)
#H1: Los residuales no se distribuyen normal (pvalue < 0,05 -> Rechazamos H0)

normvar_B2=normality.test(var_B2)
normvar_B2$jb.mul

#Sesgo -> p-value = 3.037e-05


#kurtosis -> p-value < 2.2e-16

##Se concluye que no hay normalidad, valores p < 0,05

#Procedemos a realizar la prueba de homocedasticidad de la varianza de los residuales

arch_B2 <- arch.test(var_B2, lags.multi = 3)

#H0: La varianza de los residuales es constante (pvalue >  0,05) 
#H1: La varianza de los residuales no es constante (pvalue < 0,05)

arch_B2$arch.mul
#p-value = 1 

####Modelo impulso respuesta

# 1° Veremos el impulso respuesta del tipo de cambio real, frente a variaciones de las otras variables

irf_dolar_B2=irf(var_B2, response ="tcambio_real_diff", n.ahead=12, boot=TRUE)
irf_dolar_B2

plot(irf_dolar_B2)

##Este modelo impulso respuesta nos muestra como responde el valor del dolar ante 
#un impulso de las otras variables
# Nos lo entrega, y luego la banda baja y la banda alta del modelo con un 95% conf

#Ahora el impulso respuesta de la TPM


irf_TPM_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="TPM_diff", n.ahead=12, boot=TRUE)
irf_TPM_B2

plot(irf_TPM_B2)

#Ahora el impulso respuesta del IPC frente a innovaciones en las otras variables


irf_IPC_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="IPC_diff", n.ahead=12, boot=TRUE)
irf_IPC_B2
plot(irf_IPC_B2)

#Ahora el PIB de manufacturas

irf_va_manu_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="VA_manufacturas_diff2", n.ahead=12, boot=TRUE)
irf_va_manu_B2
plot(irf_va_manu_B2)

#####PIB de servicios

irf_va_serv_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="VA_servicios_diff", n.ahead=12, boot=TRUE)
irf_va_serv_B2
plot(irf_va_serv_B2)

#####PIB minería 


irf_va_min_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="VA_minería_diff", n.ahead=12, boot=TRUE)
irf_va_min_B2
plot(irf_va_min_B2)

####Tasa de desempleo

irf_tasa_des_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="tasa_desempleo_diff", n.ahead=12, boot=TRUE)
irf_tasa_des_B2
plot(irf_tasa_des_B2)


#Ahora el impulso respuesta del precio del cobre, ante una innovación en las otras variables

irf_precio_cobre_B2=irf(var_B2, impulse = "precio_cobre_diff", response ="precio_cobre_diff", n.ahead=12, boot=TRUE)
irf_precio_cobre_B2
plot(irf_precio_cobre_B2)


#####




########################







###Impulso respuesta acumulado

irf_pcob_ac=irf(var1, response ="precio_cobre_diff", n.ahead=8, ortho = FALSE,
                cumulative=TRUE)
irf_pcob_ac
plot(irf_pcob_ac)










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
par(mfrow=c(1,1), mar=c(3,3,3,1) + .1)
plot(var_A1)

###Haremos la prueba de autocorrelación serial en los residuales

# H0: Los residuales no están correlacionados, -> p value > 0,05 Aceptar H0 -- No rechazar H0
# H1: Los residuales si están correlacionados, -> p value < 0,05 Aceptar H1 -- Rechazar H0

serialvar_A1 <- serial.test(var_A1, lags.pt = 3, type = "PT.asymptotic")
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

