### Teorema de Bayes ###
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

pacientes_positivos <- pacientes[test_resultados == "Positivo"]
prob_posterior <- sum(pacientes_positivos == "Enfermedad") / length(pacientes_positivos)
prob_posterior
