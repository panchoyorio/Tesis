
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
  scale_x_continuous(name = "Valores") +
  scale_y_continuous(name = "Periodo") + 
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
