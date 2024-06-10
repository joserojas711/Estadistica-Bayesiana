### APLICACIÓN PARA JAGS ###
### Paso 1: Instalación y configuración de las librerías 
#necesarias

#Primero, instalamos y cargamos las librerías necesarias.
#install.packages("rjags")
#install.packages("coda")

#Luego, carga las librerías.
library(rjags)
library(coda)


### Paso 2: Preparación de los datos
#Utilizamos el conjunto de datos mtcars.
data(mtcars)
head(mtcars)

# Seleccionar las variables de interés
data_list <- list(
  N = nrow(mtcars),        #N:numero de filas
  mpg = mtcars$mpg,        #mpg:variable de millas por galon
  wt = mtcars$wt           #wt:peso del automovil
)


### Paso 3: Definición del modelo
#Definimos el modelo bayesiano en el lenguaje de BUGS. Guardamos el modelo en un archivo de texto. 
model_string <- "
model {
  for (i in 1:N) {
    mpg[i] ~ dnorm(mu[i], tau)                                            #mpg sigue una distribución normal con media mu[i] y precisión tau
    mu[i] <- beta0 + beta1 * wt[i]
  }

  beta0 ~ dnorm(0, 0.001)                                                # coeficientes de la regresión, con distribuciones previas normales.
  beta1 ~ dnorm(0, 0.001)
  tau <- pow(sigma, -2)                                                   #es la precisión, que es el inverso del cuadrado de sigma, y 
  sigma ~ dunif(0, 100)                                                  #sigue una distribución uniforme.

}
"

writeLines(model_string, con = "linear_regression_model.bug")


### Paso 4: Configuración de los parámetros iniciales 
#y las cadenas

#Definimos los valores iniciales para los parámetros y 
#las cadenas de Markov.
              #Se define una función inits que genera valores iniciales
                   #aleatorios para los parámetros beta0, beta1, y sigma.

inits <- function() {
  list(
    beta0 = rnorm(1, 0, 1),
    beta1 = rnorm(1, 0, 1),
    sigma = runif(1, 0, 1)
  )
}

params <- c("beta0", "beta1", "sigma")


### Paso 5: Ejecutar el modelo en JAGS

#Ejecutamos el modelo en JAGS usando las funciones jags.model 
#y coda.samples.

# Modelo en JAGS
jags_model <- jags.model(
  file = "linear_regression_model.bug",
  data = data_list,
  inits = inits,
  n.chains = 3,              
  n.adapt = 1000                          
 
)

#Se carga el modelo en JAGS con los datos y los valores iniciales,
#utilizando 3 cadenas 
#y un periodo de adaptación de 1000 iteraciones.

# Actualizar el modelo (burn-in)
update(jags_model, 1000)  





#Se ejecutan 1000 iteraciones adicionales para permitir que las
#cadenas alcancen la convergencia (burn-in).

# Extraer muestras
jags_samples <- coda.samples(
  model = jags_model,
  variable.names = params,             
  n.iter = 5000,                   
  thin = 2
)                                  


#Se extraen muestras de las distribuciones posteriores de los parámetros, 
#con 5000 iteraciones y un thinning de 2 (es decir, se guarda una de cada dos muestras).
                                      
# Resumen de los resultados
summary(jags_samples)


### Paso 6: Interpretación de los resultados
#Finalmente, interpretamos los resultados obtenidos del análisis.

# Resumen de los resultados
print(summary(jags_samples))

# Trazas y diagnóstico de convergencia
x11()
plot(jags_samples)

#
# Extraer las medias de los parámetros
means <- summary(jags_samples)$statistics[,"Mean"]
beta0_mean <- means["beta0"]
beta1_mean <- means["beta1"]
sigma_mean <- means["sigma"]

# Mostrar los resultados
cat("Intercept:", beta0_mean, "\n")
Intercept: 37.15325 

cat("Slope (wt):", beta1_mean, "\n")   
Slope (wt): -5.307024 
                                                                    #pendiente negativa sugiere que a medida
                                                                 # que wt aumenta, el resultado disminuye.

cat("Sigma:", sigma_mean, "\n") 
Sigma: 3.169032 
                                                                      #Representa la variabilidad en los datos 