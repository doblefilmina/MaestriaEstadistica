rm(list=ls())
setwd("C:/Users/New/Documents/documentos-maestria/IAE")

datos_cardiacos <- read.csv("heart_L.txt", header=TRUE ,sep="\t")

prob_ldl_mayor_7 <- mean(datos_cardiacos$ldl > 7)
print(prob_ldl_mayor_7)

hist(datos_cardiacos[datos_cardiacos$chd==1,]$ldl, freq=F, main="histograma para personas con enfermedad coronaria", xlab="ldl", ylab="densidad")

hist(datos_cardiacos[datos_cardiacos$chd==0,]$ldl, freq=F, main="histograma para personas sin enfermedad coronaria", xlab="ldl", ylab="densidad")

empirica <- function(t,datos){
  prob_menor_t <- mean(datos<t)
  prob_menor_t
}

grilla <- seq(min(datos_cardiacos$ldl), max(datos_cardiacos$ldl), length.out=300)

distribucion_empirica_sincdh <- rep(NA, length(grilla))
distribucion_empirica_concdh <- rep(NA, length(grilla))

for(i in 1:length(grilla)){
  distribucion_empirica_sincdh[i] <- empirica( grilla[i], datos_cardiacos[datos_cardiacos$chd==0,]$ldl )
  distribucion_empirica_concdh[i] <- empirica( grilla[i], datos_cardiacos[datos_cardiacos$chd==1,]$ldl )
}

plot(grilla, distribucion_empirica_sincdh, col="blue", type="p", xlab="t", ylab="probabilidad ldl<=t", main="Empírica ldl")
points(grilla, distribucion_empirica_concdh, col="red", type="p")


##Ejercicio d)
ldl_sincdh_presente <- datos_cardiacos[datos_cardiacos$chd==0 & datos_cardiacos$famhist=="Presente",]$ldl
ldl_concdh_presente <- datos_cardiacos[datos_cardiacos$chd==1 & datos_cardiacos$famhist=="Presente",]$ldl
ldl_sincdh_ausente <- datos_cardiacos[datos_cardiacos$chd==0 & datos_cardiacos$famhist=="Ausente",]$ldl
ldl_concdh_ausente <- datos_cardiacos[datos_cardiacos$chd==1 & datos_cardiacos$famhist=="Ausente",]$ldl

boxplot(ldl_sincdh_presente,
        ldl_concdh_presente,
        ldl_sincdh_ausente,
        ldl_concdh_ausente,
        names=c("cdh=0 presente", "cdh=1 presente", "cdh=0 ausente", "cdh=1 ausente"),
        main="ldl"
        )
?bw.ucv

##Ejercicio e)
hs0 <- bw.nrd0(datos_cardiacos[datos_cardiacos$chd==0,]$ldl)
hs1 <- bw.nrd0(datos_cardiacos[datos_cardiacos$chd==1,]$ldl)

##Ejercicio f)

densidad_sinchd <- density(datos_cardiacos[datos_cardiacos$chd==0,]$ldl, kernel = "gaussian", bw = hs0)

densidad_conchd <- density(datos_cardiacos[datos_cardiacos$chd==1,]$ldl, kernel = "gaussian", bw = hs1)

plot(densidad_sinchd, main="Densidad estimada", xlab="ldl", ylab="densidad", col="blue")
lines(densidad_conchd, col="red")
