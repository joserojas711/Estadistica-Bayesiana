### Muestreo de la distribución normal ###
# Definir la distribución objetivo (normal estándar)
target_distribution <- function(x) {
  return(dnorm(x, mean = 0, sd = 1))
}

# Función de propuesta (normal)
proposal_distribution <- function(x, sigma) {
  return(rnorm(1, mean = x, sd = sigma))
}


# Algoritmo de Metropolis-Hastings
metropolis_hastings <- function(n_samples, initial_value, sigma, target_distribution, proposal_distribution) {
  samples <- numeric(n_samples)
  samples[1] <- initial_value
  
  for (t in 2:n_samples) {
    current_sample <- samples[t - 1]
    proposed_sample <- proposal_distribution(current_sample, sigma)
    
    acceptance_ratio <- min(1, target_distribution(proposed_sample) / target_distribution(current_sample))
    u <- runif(1)
    
    if (u < acceptance_ratio) {
      samples[t] <- proposed_sample
    } else {
      samples[t] <- current_sample
    }
  }
  
  return(samples)
}

# Parámetros
n_samples <- 10000
initial_value <- 0
sigma <- 1.0

# Generar muestras
samples <- metropolis_hastings(n_samples, initial_value, sigma, target_distribution, proposal_distribution)

# Graficar resultados
hist(samples, breaks = 50, probability = TRUE, col = "lightblue", main = "Muestras Metropolis-Hastings", xlab = "x")
curve(dnorm(x, mean = 0, sd = 1), col = "darkblue", lwd = 2, add = TRUE)
legend("topright", legend = c("Distribución Objetivo", "Muestras Metropolis-Hastings"),
       col = c("darkblue", "lightblue"), lwd = 2, bty = "n")
