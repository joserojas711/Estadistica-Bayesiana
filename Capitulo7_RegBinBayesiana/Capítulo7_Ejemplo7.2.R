# Importar el dataset y seleccionar columnas relevantes
#Se cargan los datos de un archivo CSV llamado `Social_Network_Ads.csv`. De estos datos, seleccionamos tres columnas: edad, salario estimado y si compraron un producto.
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
#- Se utiliza una herramienta (librería `caTools`) para dividir los datos en dos partes: 75% para entrenamiento y 25% para prueba.
# Esto significa que usaremos 75% de los datos para enseñar al modelo y 25% para ver qué tan bien aprende.
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalar valores
#**Normalizar datos:** 
# Ajustamos los valores de edad y salario para que estén en una misma escala. Esto ayuda a que el modelo funcione mejor porque todos los datos están en el mismo rango.

training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el modelo de regresión logística
#- **Crear modelo:** 
# Creamos un modelo que intenta predecir si alguien comprará basándose en su edad y salario. Este modelo se entrena usando los datos de entrenamiento.
classifier = glm(formula =  Purchased ~ .,
                 data = training_set, 
                 family = binomial)

# Predicción de los resultados con el conjunto de testing
#- Usamos el modelo para predecir las probabilidades de que las personas en el conjunto de prueba compren el producto.
# Luego, convertimos estas probabilidades en respuestas de sí o no (1 o 0) usando un umbral del 50%.

prob_pred = predict(classifier, type = "response",
                    newdata = testing_set[,-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Crear la matriz de confusión
# Comparamos las predicciones del modelo con los valores reales para ver cuántas veces acertamos y cuántas fallamos. Esto se muestra en una tabla llamada matriz de confusión.

cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#- Se crea un gráfico para mostrar cómo el modelo clasifica a las personas del conjunto de entrenamiento (los datos usados para entrenar el modelo).
# El gráfico muestra las edades y salarios, y colorea las áreas para mostrar dónde el modelo predice que las personas comprarán o no.
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'clasificación (Conjunto de entrenamiento)',
     xlab = 'Age', ylab = 'Sueldo estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
# - Similar al gráfico anterior, se crea un gráfico para mostrar cómo el modelo clasifica a las personas del conjunto de prueba (los datos usados para verificar el modelo).
# El gráfico muestra las edades y salarios, y colorea las áreas para mostrar dónde el modelo predice que las personas comprarán o no.
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'clasificación (Conjunto de testing)',
     xlab = 'Age', ylab = 'Sueldo estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))