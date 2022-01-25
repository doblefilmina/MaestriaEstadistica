
##Trabajo práctico 1
rm(list=ls())
setwd("C:/Users/New/Documents/documentos-maestria/IAE")


## 1

pp <- c(1,0,0,1,1,0)
qq <- c("female", "female", "male", "male","male")

class(pp)
class(qq)

## pp es de tipo "numeric"
## qq es de tipo "character"

pp_factor <- as.factor(pp)
qq_factor <- as.factor(qq)

class(pp_factor)
class(qq_factor)

## 2
## Para sacar el primer cuartil muestral, se deben ordenar
## los valores de menor a mayor, y encontrar el valor que
## deja un cuarto de los valores a su izquierda


## 3
## La media muestral resume alrededor de qué valor se distribuyen los datos.
## El desvío muestral resume la distancia alrededor de ese punto en la que se distribuyen los puntos

## 4
## La mediana muestral se calcula como 
## x(k+1) si n=2k+1
## x(k)/2 + x(k+1)/2 si n =k

## 6
## La diferencia entre un histograma y un gráfico de barras
## es que el primero se representa el porcentaje se representa en el
## area de la barra, mientras que en el segundo se representa en la altura de la barra.
## En el gráfico de barras, las barras se representan separadas para indicar que no hay
## cantidad entre categorías, en cambio en un histograma las barras deben estar en contacto
## las barras adyacentes indicando que la variable es continua o discreta.

## 7
## En un histograma miramos: el rango de variación de los datos (max y min)
## intervalos más frecuentes
## si la distribución es unimodal o hay más de una moda
## si la distribución es simétrica
## en caso de asimetría, a derecha o a izquierda?
## En torno a qué valor están aprox centrados los datos
## Qué tan dispersos están
## Hay datos atípicos?

## 8
## Caracterizaría estos histogramas como simétricos o asimétricos a derecha o izq,
## unimodal o multimodal

## Ejercicio 9
## Los pasos  para hacer un boxplot son:
## calcular el cuartil inferior, el cuartil superior, la mediana, y la distancia intercuartil.
## Graficamos una caja con lados en el cuartil superior e inferior, una linea en la mediana, y unos
## bigotes a 1,5 veces la distancia intercuartil. Después marcamos los datos que están más allá de los
## bigotes

par(mfrow=c(1,2))
barplot(iris$Sepal.Length)
hist(iris$Sepal.Length)


##  10
## En un boxplot miramos:
## Posición
## Dispersión
## asimetría
## longitud de colas
## puntos anómalos (outliners)


##Ejercicio 1
?airquality

datos_aire <- airquality

## 11
head(datos_aire)

str(datos_aire)
cantidad_observaciones <- length(datos_aire$Ozone)
cantidad_observaciones

## 12
nombres_variables <- c(names(datos_aire))
nombres_variables

## 13
class(datos_aire[,1])
class(datos_aire[,2])
class(datos_aire[,3])
class(datos_aire[,4])
class(datos_aire[,5])
class(datos_aire[,6])

## 14
Ozone_NA <- sum(is.na(datos_aire$Ozone))
Solar_NA <- sum(is.na(datos_aire$Solar.R))
Wind_NA <- sum(is.na(datos_aire$Wind))
Temp_NA <- sum(is.na(datos_aire$Temp))
Month_NA <- sum(is.na(datos_aire$Month))
Day_NA <- sum(is.na(datos_aire$Day))

##Las variables con datos faltantes son "Ozone"

## 15
datos_aire_reducido <- na.exclude(datos_aire)
## Cada mes hay:
hist(datos_aire_reducido[,5])
##Observaciones


## 16
hist(datos_aire_reducido$Wind)
hist(datos_aire_reducido$Temp)

## 17
boxplot(datos_aire_reducido$Wind)
boxplot(datos_aire_reducido$Temp)

## 18
plot(datos_aire_reducido$Temp, datos_aire_reducido$Wind )

## Ejercicio 2

## 19
rm(list=ls())
setwd("C:/Users/New/Documents/documentos-maestria/IAE")

## 20
datos_titanic <- read.csv("titanic.csv", header=T,sep="\t")

## 21
head(datos_titanic)
tail(datos_titanic)

## 22
View(datos_titanic)

## 23
cantidad_observaciones <- length(datos_titanic$sex)
nombres_variables <- c(names(datos_titanic))
cantidad_variables <- length(nombres_variables)

## 24
class(datos_titanic$pclass)
class(datos_titanic$survived)
class(datos_titanic$name)
class(datos_titanic$sex)
class(datos_titanic$age)
class(datos_titanic$sibsp)
class(datos_titanic$parch)
class(datos_titanic$ticket)
class(datos_titanic$fare)
class(datos_titanic$embarked)

## 25
sobrevivientes <- datos_titanic[datos_titanic$survived==1,]
View(sobrevivientes)

sobrevivientes_clase_1 <- sobrevivientes[sobrevivientes$pclass==1,]
sobrevivientes_clase_2 <- sobrevivientes[sobrevivientes$pclass==2,]
sobrevivientes_clase_3 <- sobrevivientes[sobrevivientes$pclass==3,]

sum(datos_titanic$survived)
sobrevivientes_1 <- sum(sobrevivientes_clase_1$survived)
sobrevivientes_2 <- sum(sobrevivientes_clase_2$survived)
sobrevivientes_3 <- sum(sobrevivientes_clase_3$survived)

pasajeros_clase_1 <- sum(datos_titanic$pclass==1)
pasajeros_clase_2 <- sum(datos_titanic$pclass==2)
pasajeros_clase_3 <- sum(datos_titanic$pclass==3)

proporcion_sobrevivientes_1 <- sobrevivientes_1/pasajeros_clase_1
proporcion_sobrevivientes_2 <- sobrevivientes_2/pasajeros_clase_2
proporcion_sobrevivientes_3 <- sobrevivientes_3/pasajeros_clase_3


## 26
## la proporción de supervivientes de clase 1 es 0,62, de clase 2 es 0,43 y 
## de clase 3 0,26; por lo tanto sí está asociada la clase de cabina con la 
## superviviencia

## 27
proporcion_sobrevivientes_masculinos <- sum(datos_titanic[datos_titanic$sex=="male",]$survived==1) / sum(datos_titanic$sex=="male")
proporcion_sobrevivientes_femeninos <- sum(datos_titanic[datos_titanic$sex=="female",]$survived==1) / sum(datos_titanic$sex=="female")
##proporción de supervivientes femeninos: 0.73
##proporción de supervivientes masculinos: 0.19
proporcion_sobrevivientes_masculinos

## 28
datos_titanic_filtro_tarifa <- datos_titanic[datos_titanic$fare<200,]
boxplot(datos_titanic_filtro_tarifa$fare )
hist(datos_titanic_filtro_tarifa$fare)
barplot(datos_titanic$fare)

tarifas <- datos_titanic$fare
tarifas
class(tarifas)
length(tarifas)
View(tarifas)
tarifas_ordenadas <- sort(tarifas)

boxplot(tarifas_ordenadas)
tarifas_podadas <- tarifas_ordenadas[196:1113]
length(tarifas_podadas)
1309-0.15*1309
0.85*1309
boxplot(tarifas_podadas)
hist(tarifas)
hist(tarifas_ordenadas)
hist(tarifas_podadas)
sum(is.na(tarifas))
tarifas <- tarifas[!is.na(tarifas)]
length(tarifas)

media_tarifas <- mean(tarifas)
mediana_tarifas <- median(tarifas)

media_tarifas_podada <- mean(tarifas_podadas)
mediana_tarifas_podada <- median(tarifas_podadas)

## 29
datos_titanic_filtrada <- na.omit(datos_titanic)
clase <- datos_titanic_filtrada$pclass
tarifas <- datos_titanic_filtrada$fare
datos_titanic_orden_tarifas <- datos_titanic_filtrada[with(datos_titanic_filtrada, order(datos_titanic_filtrada$fare)), ]
tarifas_podadas <- datos_titanic_orden_tarifas$fare[196:1113]
clase_podadas <- datos_titanic_orden_tarifas$pclass[196:1113]


plot(tarifas_podadas, clase_podadas)
plot(tarifas, clase)

edad <- datos_titanic_filtrada$age
plot(edad, clase)







plot(tarifas_ordenadas, rep(5, length(tarifas_ordenadas)))
hist(datos_aire_reducido[,5])
sort(pp_factor)
sort(qq_factor)

summary(qq_factor)
min(qq_factor)
range(qq_factor)
mean(qq_factor)
cor(qq_factor)
table(qq_factor)
quantile(qq_factor, 0.25)


read.csv("titanic.csv",header=T,sep="nt")
read.csv("titanic.csv",header=T,sep="'\t")

rm(list=ls())
attach(airquality)

head(airquality)

ozonecomp <- na.exclude(Ozone)
length(ozonecomp)
plot(ozonecomp, rep(5, 116))

summary(ozonecomp)

q01z <- 18
q02z <- 31.50
q03z <- 63.25
q04z <- 42.13

points(q01z, 5, col="red")
points(q02z, 5, col="blue")
points(q03z, 5, col="green")
points(q04z, 5, col="yellow")

sort(ozonecomp)



pp_numeric <- as.factor(pp)
pp_numeric
qq

boxplot(ozonecomp, horizontal=TRUE)
