# Número de iteraciones
n = 1000

# Vector para almacenar la cadena de Markov
chain = numeric(n)

# Inicialización
chain[1] = 0  # valor inicial (puede ser cualquier número entero dentro del rango de la distribución objetivo)

# Parámetros de la distribución binomial
size = 10  # número de ensayos
prob = 0.5  # probabilidad de éxito

# Función de masa de probabilidad para la distribución binomial
target_distribution <- function(x) {
  if (x >= 0 && x <= size) {
    return(dbinom(x, size, prob))  # Retorna la probabilidad de obtener 'x' éxitos en 'size' ensayos con probabilidad 'prob'
  } else {
    return(0)  # Retorna 0 fuera del rango de la distribución binomial
  }
}

# Algoritmo MCMC
for (i in 2:n) {
  # Propuesta de valor usando una distribución uniforme discreta centrada en el valor anterior
  y = sample(chain[i-1] + (-1:1), 1)  # Propuesta de un nuevo valor, uno de los tres valores posibles: anterior-1, anterior, anterior+1
  
  # Generar un número aleatorio uniforme
  u = runif(1)  # Genera un número aleatorio uniformemente distribuido entre 0 y 1
  
  # Calcular alpha
  alpha = min(1, target_distribution(y) / target_distribution(chain[i-1]))  # Calcula el factor de aceptación
  
  # Aceptar o rechazar la nueva propuesta
  if (u < alpha) {
    chain[i] = y  # Si el número aleatorio es menor que el factor de aceptación, se acepta la propuesta
  } else {
    chain[i] = chain[i-1]  # Si no, se mantiene el valor anterior en la cadena
  }
}

# Graficar la cadena de Markov
plot(chain, type = "l", main = "MCMC usando Metropolis-Hastings (Distribución Binomial)", xlab = "Iteraciones", ylab = "Valor de la cadena")
