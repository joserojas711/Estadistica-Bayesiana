### JAGS ###
#install.packages("R2jags")
library(R2jags)
modelo_bb.bugs <-
  '
model{
    for(i in 1:N){
        x[i] ~ dbern(theta)
    }
    # inicial
    theta ~ dbeta(1, 1)
}
'
theta_init <- sum(x) / N
init_theta <- function(){
  x_s <- sample(x, replace = TRUE)
  return(list(theta = sum(x_s) / N))
}
cat(modelo_bb.bugs, file = 'modelo_bb.bugs')

jags_fit <- jags(
  model.file = "modelo_bb.bugs",    # modelo de JAGS
  inits = init_theta,   # valores iniciales
  data = list(x = x, N = N),    # lista con los datos
  parameters.to.save = c("theta"),  # parámetros por guardar
  n.chains = 1,   # número de cadenas
  n.iter = 1000,    # número de pasos
  n.burnin = 500   # calentamiento de la cadena
)
head(jags_fit$BUGSoutput$summary)
traceplot(jags_fit, varname = "theta")