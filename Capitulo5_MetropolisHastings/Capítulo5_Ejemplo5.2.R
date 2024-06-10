### Muestreo de la distribución normal estándar ###
# Función de la distribución objetivo (Normal estándar)
target_density <- function(x) {
  return(dnorm(x, mean = 0, sd = 1))
}

# Algoritmo de Metropolis-Hastings
metropolis_hastings <- function(iterations, sigma) {
  # Inicialización
  x <- numeric(iterations)
  x[1] <- 0  # punto inicial
  
  for (t in 2:iterations) {
    # Generar un candidato a partir de la distribución de propuesta
    x_prime <- rnorm(1, mean = x[t-1], sd = sigma)
    
    # Calcular la probabilidad de aceptación
    alpha <- min(1, target_density(x_prime) / target_density(x[t-1]))
    
    # Generar un número aleatorio uniforme
    u <- runif(1)
    
    # Decidir si se acepta o rechaza el candidato
    if (u <= alpha) {
      x[t] <- x_prime
    } else {
      x[t] <- x[t-1]
    }
  }
  
  return(x)
}

# Parámetros
iterations <- 10000  # número de iteraciones
sigma <- 1  # desviación estándar de la distribución de propuesta

# Ejecutar el algoritmo de Metropolis-Hastings
samples <- metropolis_hastings(iterations, sigma)

# Visualizar los resultados
hist(samples, breaks = 50, probability = TRUE, main = "Histograma de las muestras generadas",
     xlab = "Valor", col = "skyblue")

# Superponer la distribución objetivo
curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)


