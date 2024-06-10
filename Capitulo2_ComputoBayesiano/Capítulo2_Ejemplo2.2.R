### Ejemplo probabilidad subjetiva ###
#install.packages("brms")
library("brms")
library("ggplot2")

# Generar 1000 días simulados
n_simulaciones <- 1000

# Probabilidad subjetiva de lluvia
prob_lluvia <- 0.7

# Simulación de días lluviosos
dias_lluvia <- rbinom(n_simulaciones, size = 1, prob = prob_lluvia)

# Calcular la proporción de días lluviosos
prop_lluvia <- mean(dias_lluvia)

# Crear un dataframe con los resultados
simulacion_df <- data.frame(Dia = 1:n_simulaciones, Lluvia = dias_lluvia)

### PROPORCIÓN DE DÍAS LLUVIOSOS
ggplot(simulacion_df, aes(x = Lluvia)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", color = "black") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  geom_vline(xintercept = 0.7, color = "red", linetype = "dashed") +
  labs(x = "Probabilidad de lluvia", y = "Densidad", title = "Distribución de la probabilidad de días lluviosos") +
  theme_minimal()

#Ejemplo Teorema de Bayes

# Definimos las probabilidades previas
probabilidades_previas <- c("Enfermedad" = 0.01, "NoEnfermedad" = 0.99)

# Definimos las probabilidades condicionales
# P(Test+|Enfermedad) y P(Test+|NoEnfermedad)
probabilidades_condicionales <- c("TestPositivo_Dado_Enfermedad" = 0.9, 
                                  "TestPositivo_Dado_NoEnfermedad" = 0.2)

# Realizamos la simulación de pacientes
set.seed(128) # Para reproducibilidad
pacientes <- sample(c("Enfermedad", "NoEnfermedad"), size = 1000, 
                    replace = TRUE, prob = probabilidades_previas)

# Simulamos los resultados de los tests
test_resultados <- sapply(pacientes, function(enfermedad) {
  if (enfermedad == "Enfermedad") {
    return(sample(c("Positivo", "Negativo"), size = 1, prob = c(0.9, 0.1)))
  } else {
    return(sample(c("Positivo", "Negativo"), size = 1, prob = c(0.2, 0.8)))
  }
})
############################################################################

#Ejemplo Método de Simulación Monte Carlo

# Definir el modelo en Stan
model_code <- "
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
}
"

# Generar algunos datos de ejemplo
set.seed(123)
N <- 100
x <- rnorm(N, 0, 1)
alpha <- 2
beta <- 1.5
sigma <- 1
y <- alpha + beta * x + rnorm(N, 0, sigma)

# Preparar los datos para Stan
data_list <- list(N = N, x = x, y = y)

# Compilar el modelo en Stan
stan_model <- stan_model(model_code = model_code)

# Ajustar el modelo utilizando MCMC
fit <- sampling(stan_model, data = data_list, iter = 2000, chains = 4)

# Imprimir los resultados
print(fit)

# Extraer las muestras
samples <- extract(fit)

# Graficar los resultados
plot(fit)

# Resumen de estadísticas para los parámetros
summary(samples$alpha)
summary(samples$beta)
summary(samples$sigma)

# Distribuciones posteriores
alpha_samples <- samples$alpha
beta_samples <- samples$beta
sigma_samples <- samples$sigma

# Distribución posterior para alpha
ggplot(data.frame(alpha = alpha_samples), aes(x = alpha)) +
  geom_density(fill = "skyblue") +
  labs(title = "Posterior distribution of alpha")

# Distribución posterior para beta
ggplot(data.frame(beta = beta_samples), aes(x = beta)) +
  geom_density(fill = "indianred1") +
  labs(title = "Posterior distribution of beta")

# Distribución posterior para sigma
ggplot(data.frame(sigma = sigma_samples), aes(x = sigma)) +
  geom_density(fill = "palegreen2") +
  labs(title = "Posterior distribution of sigma")

