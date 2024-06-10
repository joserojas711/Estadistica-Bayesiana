#Paso 1: Cargar la base de datos.
# datos 
y <- c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
# tamaño de la muestra
(n <- length(y))

#Paso 2: Estadísticos suficientes.
(mean_y <- mean(y))

#Paso 3: Definir hiperparametros
mu0 <- 1.9 
t20 <- 0.5^2
s20 <- 0.01 
nu0 <- 1

#Paso 4: Numero de muestras.
B <- 100000

#Paso 5: Almacenar las muestras en una matriz.
PHI <- matrix(data = NA, nrow = B, ncol = 2)
colnames(PHI) <- c("theta", "isig2")

#Paso 6: Algoritmo muestreador de Gibbs
# inicializar
set.seed(1234)
isig2 <- rgamma(n = 1, shape = nu0/2, rate = nu0*s20/2)
# cadena
set.seed(1234)
for(b in 1:B) {
  # actualizar theta
  t2n   <- 1/(1/t20 + n*isig2)      
  mun   <- t2n*(mu0/t20 + isig2*sum_y)
  theta <- rnorm(n = 1, mean = mun, sd = sqrt(t2n))
  # actualizar sigma^2
  nun   <- nu0 + n
  s2n   <- (nu0*s20 + sum((y - theta)^2))/nun
  isig2 <- rgamma(n = 1, shape = nun/2, rate = nun*s2n/2)
  # almacenar
  PHI[b,] <- c(theta, isig2)
  # progreso
  if (b%%ncat == 0) 
    cat(100*round(b/B, 1), "% completado ... \n", sep = "")
}


#Paso 7: Graficar las distribuciones a posterior y marginal
#Distribuciones posterior y marginales
Par(mfrow=c(2,2),mar=c(2.75,3,.5,.5),mgp=c(1.70,.70,0))
Sseq <- 1:10000
# distribucion conjunta
plot(PHI[sseq,1], PHI[sseq,2], pch=”.”, xlim=range(PHI[,1]),ylim=c(0,225), xlab=expression(theta), ylab=expression(tilde(sigma)^2))
# distribucion conjunta
plot(PHI[sseq,1], 1/PHI[sseq,2], pch=”.”, xlim=range(PHI[,1]),ylim=c(0,0.15), xlab=expression(theta), ylab=expression(sigma^2))
# theta
plot(density(PHI[,1], adj=2), xlab=expression(theta), main=””, xlim=c(1.55,2.05), ylab=expression(paste(italic(“p(“), theta,”|”,italic(y[1]),”…”,italic(y[n]),”)”,sep=””)))
# precision
plot(density(PHI[,2],adj=2), xlab=expression(tilde(sigma)^2),main=””, ylab=expression(paste(italic(“p(“),tilde(sigma)^2,”|”,italic(y[1]),”…”,italic(y[n]),”)”,sep=””))) 
