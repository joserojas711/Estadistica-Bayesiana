### Ejemplo Bioensayo ###
#install.packages("brms")
library("brms")
library("ggplot2")

dose <- c(-0.863, -0.296, -0.053, 0.727)
n <- c(5, 5, 5, 5)  # número de plantas por dosis
y <- c(0, 1, 3, 5)  # número de éxitos por dosis
data_list <- data.frame(dose, n, y)
#
fit_brms <- brm(
  bf(y | trials(n) ~ log(dose)), 
  data = data_list, 
  family = binomial(link = "logit"), 
  prior = c(
    set_prior("uniform(-10, 10)", class = "b"),
    set_prior("uniform(-10, 10)", class = "Intercept")
  ),
  iter = 2000, 
  chains = 4, 
  seed = 123
)
#
posterior_samples_df <- posterior_samples(fit_brms)


####### DISTRIBUCIÓN POSTERIOR MCMC #######
ggplot() +
  geom_point(data = posterior_samples_df, aes(x = b_Intercept, y = b_logdose), alpha = 0.4, color = "blue") +
  labs(title = "Distribución Posterior Combinada de los Coeficientes",
       x = "alfa (Intercepción)",
       y = "beta (log(dose))") +
  theme_minimal()
#
mean_intercept <- mean(posterior_samples_df$b_Intercept)
sd_intercept <- sd(posterior_samples_df$b_Intercept)
#
mean_logdose <- mean(posterior_samples_df$b_logdose)
sd_logdose <- sd(posterior_samples_df$b_logdose)
#
x_range <- c(mean_intercept - 3 * sd_intercept, mean_intercept + 3 * sd_intercept)
y_range <- c(mean_logdose - 3 * sd_logdose, mean_logdose + 3 * sd_logdose)


####### DISTRIBUCIÓN POSTERIOR (COEFICIENTES) MCMC #######
ggplot(posterior_samples_df, aes(x = b_Intercept, y = b_logdose)) +
  geom_density_2d() +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  labs(title = "Densidad Posterior Conjunta de los Coeficientes", 
       x = "Intercept", 
       y = "log(dose)") +
  theme_minimal()
#
posterior_intercept <- posterior_samples_df$b_Intercept
posterior_logdose <- posterior_samples_df$b_logdose
#
inverse_cdf <- function(samples, u) {
  ecdf_fun <- ecdf(samples)
  quantile(samples, probs = u)
}
#
set.seed(123)
n_samples <- 1000
u <- runif(n_samples)
samples_intercept <- inverse_cdf(posterior_intercept, u)
samples_logdose <- inverse_cdf(posterior_logdose, u)
#
samples_df <- data.frame(samples_intercept, samples_logdose)

####### DISTRIBUCIÓN ACUMULADA POSTERIOR MCMC #######
ggplot(samples_df, aes(x = samples_intercept, y = samples_logdose)) +
  geom_point(alpha = 0.4) +
  labs(title = "Muestras desde la Distribución Posterior",
       x = "Intercept", 
       y = "log(dose)") +
  theme_minimal()