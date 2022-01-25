rm(list=ls())
setwd("C:/Users/New/Documents/documentos-maestria/IAE")

enes <- c(6,10,20,40,80,200)

## Xn tiene distribución uniforme (0, tita), la función de distribución es fn(x)= 1/tita entre 0 y tita

## Para tita = 3

tita <- 3

distribucion_n_6 <- runif(enes[1], 0, tita)
distribucion_n_10 <- runif(enes[2], 0, tita)
distribucion_n_20 <- runif(enes[3], 0, tita)
distribucion_n_40 <- runif(enes[4], 0, tita)
distribucion_n_80 <- runif(enes[5], 0, tita)
distribucion_n_200 <- runif(enes[6], 0, tita)

estimador_max_6 <- max(distribucion_n_6)
estimador_prom_6 <- 2*mean(distribucion_n_6)

estimador_max_10 <- max(distribucion_n_10)
estimador_prom_10 <- 2*mean(distribucion_n_10)

estimador_max_20 <- max(distribucion_n_20)
estimador_prom_20 <- 2*mean(distribucion_n_20)

estimador_max_40 <- max(distribucion_n_40)
estimador_prom_40 <- 2*mean(distribucion_n_40)

estimador_max_80 <- max(distribucion_n_80)
estimador_prom_80 <- 2*mean(distribucion_n_80)

estimador_max_200 <- max(distribucion_n_200)
estimador_prom_200 <- 2*mean(distribucion_n_200)
  
  

prueba <- data.frame("col1"=c(1), "col2"=as.array(c(1,2,3)))

as.array(c(1,2,3))

prueba <- data.frame()
prueba[1,1] <- 1
prueba[1,2] <- c(1,2,3)

estimador <- function(n, m){
  data <- data.frame("ensayo"=seq(1,m), "estimador_max"=rep(NA, m), "estimador_prom"=rep(NA, m))
  for(i in 1:m){
    distribucion <- runif(n, 0, tita)
    data$estimador_max[i] <- max(distribucion)
    data$estimador_prom[i] <- 2*mean(distribucion)
  }
  return(data) }

ensayos_n_6 <- estimador(6,1000)
ensayos_n_10 <- estimador(10,1000)
ensayos_n_20 <- estimador(20,1000)
ensayos_n_40 <- estimador(40,1000)
ensayos_n_80 <- estimador(80,1000)
ensayos_n_200 <- estimador(200,1000)

ECM_n_6 <- c(mean( (ensayos_n_6$estimador_max-tita)**2 ), mean( (ensayos_n_6$estimador_prom - tita)**2 ) )
ECM_n_10 <- c(mean( (ensayos_n_10$estimador_max-tita)**2 ), mean( (ensayos_n_10$estimador_prom - tita)**2 ) )
ECM_n_20 <- c(mean( (ensayos_n_20$estimador_max-tita)**2 ), mean( (ensayos_n_20$estimador_prom - tita)**2 ) )
ECM_n_40 <- c(mean( (ensayos_n_40$estimador_max-tita)**2 ), mean( (ensayos_n_40$estimador_prom - tita)**2 ) )
ECM_n_80 <- c(mean( (ensayos_n_80$estimador_max-tita)**2 ), mean( (ensayos_n_80$estimador_prom - tita)**2 ) )
ECM_n_200 <- c(mean( (ensayos_n_200$estimador_max-tita)**2 ), mean( (ensayos_n_200$estimador_prom - tita)**2 ) )

ECM_n_6 <- mean( (ensayos_n_6$estimador_max-tita)**2 )

View(ensayos_n_6)

  ECM_estimador_max <- c(ECM_n_6[1], ECM_n_10[1], ECM_n_20[1], ECM_n_40[1], ECM_n_80[1], ECM_n_200[1] )
  ECM_estimador_prom <- c(ECM_n_6[2], ECM_n_10[2], ECM_n_20[2], ECM_n_40[2], ECM_n_80[2], ECM_n_200[2] )

plot(enes, ECM_estimador_max)
points(enes, ECM_estimador_prom, col="green")

par(mfrow=c(1,1))
hist(ensayos_n_6$estimador_max, freq = FALSE)
hist(ensayos_n_10$estimador_max, freq = F)
hist(ensayos_n_20$estimador_max, freq = F)
hist(ensayos_n_40$estimador_max, freq = F)
hist(ensayos_n_80$estimador_max, freq = F)
hist(ensayos_n_200$estimador_max, freq = F)

hist(ensayos_n_6$estimador_prom, freq = F)
hist(ensayos_n_10$estimador_prom, freq = F)
hist(ensayos_n_20$estimador_prom, freq = F)
hist(ensayos_n_40$estimador_prom, freq = F)
hist(ensayos_n_80$estimador_prom, freq = F)
hist(ensayos_n_100$estimador_prom, freq = F)

boxplot(ensayos_n_6$estimador_prom)
boxplot(ensayos_n_6$estimador_max)

?hist
View(ensayos_n_6)
?return
?cbind

distribuciones_n <- data.frame( n=enes, distribucion=rep(rep(0, length(enes)), length(enes)) )

for (i in 1: length(enes) ){
  distribuciones_n$distribucion[i] <- runif(distribuciones_n$n[i], 1,3)
}
View(distribuciones_n)
runif(1,3, distribuciones_n$n[1]
      
      
      distribuciones_n$distribucion[1] <- c(6,10,20,40,80,200)
t <- 0
t <- c(6,10,20,40,80,200)
distribuciones_n$distribucion
