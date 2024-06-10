#Paso 1: Cargar la base de datos
library(MASS)
data(mtcars)

# Definir variables
y <- mtcars$mpg
x <- mtcars$wt
n <- length(y)

#Paso 2: Definir hiperparámetros
beta_0 <- 0
beta_1 <- 0
tau_0 <- 1
alpha_0 <- 1
beta_sigma_0 <- 1

#Paso 3: Inicialización de parámetros
beta <- c(0, 0)
sigma2 <- 1

#Paso 4: Definir el número de iteraciones
iterations <- 10000

#Paso 5: Preparar almacenamiento para las muestras
beta_samples <- matrix(NA, nrow = iterations, ncol = 2)
sigma2_samples <- numeric(iterations)

#Paso 6: Implementar el muestreador de Gibbs
for (iter in 1:iterations) {
  # Muestrear beta dado sigma2 y los datos
  X <- cbind(1, x)
  V_beta <- solve(t(X) %*% X / sigma2 + diag(1 / c(tau_0, tau_0)))
  m_beta <- V_beta %*% (t(X) %*% y / sigma2 + c(beta_0 / tau_0, beta_1 / tau_0))
  beta <- mvrnorm(1, mu = m_beta, Sigma = V_beta)
  
  # Muestrear sigma2 dado beta y los datos
  alpha_n <- alpha_0 + n / 2
  beta_n <- beta_sigma_0 + sum((y - X %*% beta)^2) / 2
  sigma2 <- 1 / rgamma(1, shape = alpha_n, rate = beta_n)
  
  # Guardar muestras
  beta_samples[iter, ] <- beta
  sigma2_samples[iter] <- sigma2
}

#Paso 7: Remover el burn-in
burn_in <- 1000
beta_samples <- beta_samples[-(1:burn_in), ]
sigma2_samples <- sigma2_samples[-(1:burn_in)]

#Paso 8: Calcular estimaciones de los parámetros
beta_estimates <- apply(beta_samples, 2, mean)
sigma2_estimate <- mean(sigma2_samples)

cat("Estimación de beta:", beta_estimates, "\n")
cat("Estimación de sigma^2:", sigma2_estimate, "\n")


#Paso 9: Graficar las cadenas de Markov y las distribuciones a posteriori
par(mfrow = c(3, 1))
plot(beta_samples[, 1], type = "l", main = expression(beta[0]), ylab = "Valor")
plot(beta_samples[, 2], type = "l", main = expression(beta[1]), ylab = "Valor")
plot(sigma2_samples, type = "l", main = expression(sigma^2), ylab = "Valor")

par(mfrow = c(3, 1))
hist(beta_samples[, 1], main = expression(beta[0]), xlab = "Valor", breaks = 30)
hist(beta_samples[, 2], main = expression(beta[1]), xlab = "Valor", breaks = 30)
hist(sigma2_samples, main = expression(sigma^2), xlab = "Valor", breaks = 30)



