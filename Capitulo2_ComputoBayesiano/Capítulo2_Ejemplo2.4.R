### Simulación Monte Carlo ###
#install.packages("brms")
library("rstan")
library("ggplot2")

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

### INTERVALO DE CREDIBILIDAD ###
plot(fit)


# Resumen de estadísticas para los parámetros
summary(samples$alpha)
summary(samples$beta)
summary(samples$sigma)

# Distribuciones posteriores
alpha_samples <- samples$alpha
beta_samples <- samples$beta
sigma_samples <- samples$sigma

### DISTRIBUCIONES POSTERIORES
# ALPHA
ggplot(data.frame(alpha = alpha_samples), aes(x = alpha)) +
  geom_density(fill = "skyblue") +
  labs(title = "Posterior distribution of alpha")

# BETA
ggplot(data.frame(beta = beta_samples), aes(x = beta)) +
  geom_density(fill = "indianred1") +
  labs(title = "Posterior distribution of beta")

# SIGMA
ggplot(data.frame(sigma = sigma_samples), aes(x = sigma)) +
  geom_density(fill = "palegreen2") +
  labs(title = "Posterior distribution of sigma")

