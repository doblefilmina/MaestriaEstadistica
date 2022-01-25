
# Borro todo
rm(list=ls())

# Defino directorio de trabajo
setwd("C:/Users/New/Documents/documentos-maestria/IAE")

##Ejercicio 1
datos_altura <- read.csv("alturas_n_500.csv")

##Ejercicio 2
str(datos_altura)
nombres <- names(datos_altura)

##Ejercicio 3
hist(datos_altura$altura, freq = FALSE, ylim=c(0,0.07), main="Histograma de alturas", ylab="Densidad", xlab="altura en cm")
##Se observan dos modas. Se debe a las alturas más probables femeninas y masculinas

##Ejercicio 4
ventana_optima <- bw.ucv(datos_altura$altura)
lines(density(datos_altura$altura))
##En la función de densidad estimada se observan nuevamente dos modas, también correspondiente
## a las alturas más frecuentes de parte femenina y masculina respectivamente.

View(datos_altura)

##Ejercicio 5
altura_mujeres <- datos_altura[(datos_altura$genero=="F"),]$altura
ventana_optima_mujeres <- bw.ucv(altura_mujeres)
altura_hombres <- datos_altura[(datos_altura$genero=="M"),]$altura
ventana_optima_hombres <- bw.ucv(altura_hombres)

##Ejercicio 6
hist(altura_hombres, freq = FALSE, ylim=c(0,0.15), main="Histograma de alturas hombres", ylab="Densidad", xlab="altura en cm")
lines(density(altura_hombres))
hist(altura_mujeres, freq = FALSE, ylim=c(0,0.15), main="Histograma de alturas mujeres", ylab="Densidad", xlab="altura en cm")
lines(density(altura_mujeres))


##EJercicio 7
## Yo creo que se puede predecir la altura de de una persona con la moda de la altura en cada género.
## Para una mujer 1,59 ; para el hombre 1,71

##Ejercicio 8
altura_hijas_pequeña <- datos_altura[datos_altura$genero=="M" & datos_altura$contextura_madre=="bajita",]$altura

hist(altura_hijas_pequeña, freq = FALSE)
lines(density(altura_hijas_pequeña))

densidad_rectangular_1 <- density(datos_altura$altura, kernel="rectangular", bw=5)
densidad_rectangular_2 <- density(datos_altura$altura, kernel="rectangular", window=5)
densidad_rectangular_3 <- density(datos_altura$altura, kernel="rectangular", window=ventana_optima)


densidad_gaussiano <- density(datos_altura$altura, kernel="gaussian", window=5)
densidad_gaussiano_2 <- density(datos_altura$altura, kernel="gaussian", window=ventana_optima)



densidad

lines(densidad_rectangular_1, col="red", type="l")
lines(densidad_rectangular_2, col="blue", type="l")
lines(densidad_rectangular_3, col="yellow", type="l")

lines(densidad_gaussiano, col="green", type="l")
lines(densidad_gaussiano, col="orange", type="l")



densidad_h2 <- density(df$x, kernel = "gaussian", bw = 2)
densidad_h4 <- density(df$x, kernel = "gaussian", bw = 4)
densidad_h1 <- density(df$x, kernel = "gaussian", bw = 1)
densidad_h0.5 <- density(df$x, kernel = "gaussian", bw = 0.5)



grilla <- seq(min(df$x), max(df$x), length.out = 100)

hist(df$x, breaks = 30,freq = FALSE, ylim=c(0,0.30))
lines(densidad_h0.5, col = "purple")
lines(densidad_h1, col = "red")
lines(densidad_h2, col = "blue")
lines(densidad_h4, col = "green")

approxfun(densidad_h0.5)(16)
approxfun(densidad_h0.5)(18)
approxfun(densidad_h0.5)(20)
approxfun(densidad_h0.5)(22)



plot(densidad_h0.5)


#####
altura_madres_mujeres <- datos_altura[(datos_altura$genero=="F"),]$altura_madre
hist(altura_madres_mujeres)



altura_madres_hombres <- datos_altura[(datos_altura$genero=="M"),]$altura_madre
hist(altura_madres_hombres)
plot(altura_madres_hombres, altura_hombres, col="red", ylim=c(145,190), xlab="altura madres", ylab="altura hijes")
points(altura_madres_mujeres, altura_mujeres, col="green")

which(altura_madres_hombres==156)

abs(altura_madres_hombres[c(8,12,26)] - 1000)

madres_155_157 <- which(abs(altura_madres_hombres-156)<=2)
madres_155_157
hijos_155_157 <- altura_hombres[madres_155_157]
mean(hijos_155_157)

alturaMadres_alturaHijos <- datos_altura[(datos_altura$genero=="F"),]
alturaMadres_alturaHijos <- alturaMadres_alturaHijos[, c(-2,-3)]
View(alturaMadres_alturaHijos)

ordenado <- alturaMadres_alturaHijos[order(alturaMadres_alturaHijos$altura_madre),]

View(ordenado)

k_vecines <- function(k, datos, objetivo){
  ###recibe el número de vecinos k, un vector ordenado y devuelve las posiciones de les k vecines más cercanos
  
  posiciones_objetivo <- which( abs(datos-objetivo) == min( abs(datos-objetivo) )  )
  
    posiciones <- c( seq(min(posiciones_objetivo)-k, posiciones_objetivo-1) , posiciones_objetivo, seq(max(posiciones_objetivo)+1, posiciones_objetivo+k) )
    distancias <- abs(datos[posiciones] - objetivo)
    t <- length(posiciones)
    while (t>k+1) {
      maximo <- c( which(distancias == max(distancias)) )
      if(t-length(maximo) >= k+1){
        t <- t-length(maximo)
        posiciones <- posiciones[-maximo]
        distancias <- distancias[-maximo]
      } else {
        t <- k-1
      }
      
    }
    posiciones
  }
  
  posiciones <- k_vecines(7,ordenado$altura_madre,156 )

  promedio <- mean(ordenado$altura[posiciones])  
  ordenado$altura[posiciones]
  posiciones
  
  predigo_altura_masculino <- function(altura masc,altura madre masc,altura mama nueva,h){
    
  }
  