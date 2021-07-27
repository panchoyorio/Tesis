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

#Importamos la base de datos
ruta_excel <- "C:\\Users\\fcoyo\\Desktop\\Tesis\\BBDD Consolidada (version 1).xlsb.xlsx"

Base_datos <- read_excel(ruta_excel,
                         sheet = "DATOS.TS")

Base_datos.ts = ts(Base_datos, start = 1985, frequency = 4)
Base_datos.ts
plot(Base_datos.ts)

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
tasa_IPCX = ts(Base_datos.ts[,2], start=1985, freq=4)
tasa_IPCX
precio_cobre = ts(Base_datos.ts[,8], start=1985, freq=4)
precio_cobre

#Graficos 
plot(tcambio_real, ylab="Precio", xlab="Trimestres", main="Tipo de Cambio Real en Chile", col="blue")

plot(tcambio_dolarobs, ylab="Precio", xlab="Trimestres", main="Valor del dolar observado en Chile", col="blue")

plot(tasa_TPM, ylab="Tasa", xlab="Trimestres", main="TPM", col="blue")

plot(tasa_captación, ylab="Tasa", xlab="Trimestres", main="Tasa de Captación", col="blue")

plot(tasa_colocación, ylab="Tasa", xlab="Trimestres", main="Tasa de Colocación", col="blue")

plot(tasa_IPC, ylab="Índice", xlab="Trimestres", main="IPC", col="blue")

plot(tasa_IPCX, ylab="Índice", xlab="Trimestres", main="IPC Subyacente", col="blue")

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
acf(tasa_IPCX)
acf(precio_cobre)
