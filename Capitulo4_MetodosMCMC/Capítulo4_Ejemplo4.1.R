### (Distribución normal estándar N(0,1)) ###
#Se busca generar numeros pseudoaleatorios utilizando el método MCMC para ver si los datos generados se aproximan a los datos reales
# Número de iteraciones
n = 1000

# Vector para almacenar la cadena de Markov
alea = numeric(n)

# Inicialización
alea[1] = 0  # valor inicial (mu)

# Algoritmo MCMC 
#Para cada iteración(desde la segunda hasta la última)
for (i in 2:n) {
  # Propuesta de valor usando una distribución 
  #normal centrada en el valor anterior Con desviación estandar 0.5
  y = rnorm(1, alea[i-1], 0.5)
  
  # Generar un número aleatorio uniforme
  u = runif(1)
  
  # Calcular alpha
  alpha = min(1, (dnorm(y) * dnorm(alea[i-1], y, 0.5))
          / (dnorm(alea[i-1]) * dnorm(y, alea[i-1], 0.5)))
  
  # Aceptar o rechazar la nueva propuesta comparando 'u' con 'alpha'
  if (u < alpha) {
    alea[i] = y
  } else {
    alea[i] = alea[i-1]
  }
}

#Se  grafican los residuales para ver si se aproximan a la normal
# Graficar la cadena de Markov
plot(alea, type = "l", main = "MCMC Continuo usando Metropolis-Hastings
(Distribución normal estándar N(0,1))", xlab = "Iteraciones", ylab = "Valor de la cadena")
#Podemos observar que en efecto, los residuaes si se aproximan a la normal
