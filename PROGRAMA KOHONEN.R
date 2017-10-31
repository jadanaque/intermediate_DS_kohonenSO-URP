rm(list=ls())
# Autor : Jose Cardenas

# Se calcula una distancia ponderada sobre todas las capas 
# para determinar las unidades ganadoras durante el entrenamiento. 
# Las funciones som y xyf son simplemente envolturas para superestructuras 
# con una y dos capas, respectivamente

#USO DE LA FUNCI?N SOM DE VINOS
datos=read.table("datas/VINOS.txt",header=T)
datos
set.seed(7)
vinos.sc=scale(datos[,1:13])
library(kohonen)
vino.som=som(vinos.sc, grid = somgrid(5,5,"hexagonal"))  # Increase number of iterations. See plot of type = "changes".
# vino.som=som(vinos.sc, grid = somgrid(5,5,"hexagonal"), rlen = 500)  # This yields much better results
names(vino.som)
summary(vino.som)
vino.som$unit.classif #unidades ganadoras para todos los objetos de datos
vino.som$codes #una lista de matrices que contienen vectores de libro de c?digos.
plot(vino.som, main="Datos de vino")
# plot(vino.som, type = "quality")

#USO DE LA FUNCI?N xyf DE VINOS
#attach(datos)
set.seed(7)
kohmap = xyf(vinos.sc, classvec2classmat(datos$clase),grid = somgrid(5, 5, "hexagonal"), rlen=100)
plot(kohmap, type="codes",main=c("Distribuci?n de variables","Clases de c?digos"))
plot(kohmap, type="mapping",col=datos$clase+1,main="Mapa de clases")
plot(kohmap, type = "mapping",labels = datos$clase, col = datos$clase+1, main = "Mapa de clases")
plot(kohmap,type="counts",main="Diagrama de conteos")
plot(kohmap, type = "quality", labels = datos$clase, col = datos$clase + 1, main = "Mapa de calidad")





