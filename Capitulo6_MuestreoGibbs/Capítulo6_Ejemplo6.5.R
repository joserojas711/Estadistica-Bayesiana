# Paso 1: Cargar los datos
data(iris)

# Seleccionar dos variables
x <- iris$Sepal.Length
y <- iris$Sepal.Width

# Paso 2: Centrar los datos
x <- x - mean(x)
y <- y - mean(y)

# Paso 3: Función para la distribución condicional de X dado Y
cond_x_given_y <- function(y, mu_x, mu_y, sigma_x, sigma_y, rho) {
  mu <- mu_x + rho * (sigma_x / sigma_y) * (y - mu_y)
  sigma <- sqrt((1 - rho^2) * sigma_x^2)
  rnorm(1, mean = mu, sd = sigma)
}

# Paso 4: Función para la distribución condicional de Y dado X
cond_y_given_x <- function(x, mu_x, mu_y, sigma_x, sigma_y, rho) {
  mu <- mu_y + rho * (sigma_y / sigma_x) * (x - mu_x)
  sigma <- sqrt((1 - rho^2) * sigma_y^2)
  rnorm(1, mean = mu, sd = sigma)
}

# Paso 5: Parámetros iniciales
mu_x <- 0
mu_y <- 0
sigma_x <- 1
sigma_y <- 1
rho <- 0.5

# Paso 6: Definir el número de iteraciones
n_iter <- 10000

# Paso 7: Almacenar resultados y aplicar el algoritmo de Gibbs
samples <- matrix(NA, n_iter, 2)
colnames(samples) <- c("X", "Y")

# Valores iniciales de las variables
x_curr <- 0
y_curr <- 0

# Algoritmo de Gibbs
set.seed(123) # Para reproducibilidad

for (i in 1:n_iter) {
  x_curr <- cond_x_given_y(y_curr, mu_x, mu_y, sigma_x, sigma_y, rho)
  y_curr <- cond_y_given_x(x_curr, mu_x, mu_y, sigma_x, sigma_y, rho)
  samples[i, ] <- c(x_curr, y_curr)
}

#Paso 8: Graficar
# Plot de las muestras generadas
library(ggplot2)

samples_df <- as.data.frame(samples)
ggplot(samples_df, aes(x = X, y = Y)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Muestras generadas por el Algoritmo de Gibbs",
       x = "X",
       y = "Y")

