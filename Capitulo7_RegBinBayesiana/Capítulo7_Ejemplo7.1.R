setwd("C:\\Users\\Ruben\\Documents\\bayes-app")
library(tidyverse)
library(caret)
library(GGally)
library(ggplot2)
library(corrplot)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(rstanarm)
options(mc.cores = 8)
library(loo)
library(projpred)
SEED=14124869
install.packages("projpred")

# file preview shows a header row
diabetes <- read.csv("diabetes.csv", header = TRUE)

# first look at the data set using summary() and str() to understand what type of data are you working
# with
summary(diabetes)

#preprocesamiento
# eliminando esas filas de observación con 0 en cualquiera de las variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}

# escalar las covariables para facilitar la comparación de los 
#coeficientes posteriores
for (i in 1:8) {
  diabetes[i] <- scale(diabetes[i])
}

# modificar ligeramente los nombres de las columnas de datos para 
#escribir más fácilmente
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

n=dim(diabetes)[1]
p=dim(diabetes)[2]
str(diabetes)

print(paste0("number of observations = ", n))
print(paste0("number of predictors = ", p))

corrplot(cor(diabetes[, c(9,1:8)]))

diabetes$outcome <- factor(diabetes$outcome)
# preparing the inputs
x <- model.matrix(outcome ~ . - 1, data = diabetes)
y <- diabetes$outcome
(reg_formula <- formula(paste("outcome ~",
                              paste(names(diabetes)[1:(p-1)], 
                                    collapse = " + "))))

#Modelo de regresion logistica bayesiana
# Instalar y cargar el paquete brms si no está instalado
# Instalar y cargar el paquete rstanarm si no está instalado
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  install.packages("rstanarm")
}
library(rstanarm)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(reg_formula, data = diabetes,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0)

pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

round(coef(post1), 2)
round(posterior_interval(post1, prob = 0.9), 2)

(loo1 <- loo(post1, save_psis = TRUE))

post0 <- update(post1, formula = outcome ~ 1, QR = FALSE, refresh=0)

(loo0 <- loo(post0))
loo_compare(loo0, loo1)


loo0
# Predicted probabilities
linpred <- posterior_linpred(post1)
preds <- posterior_epred(post1)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)

# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,
                as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5, as.integer(y[y==1]))))/2,2)

# LOO predictive probabilities
ploo=E_loo(preds, loo1$psis_object, type="mean", 
           log_ratios = -log_lik(post1))$value
# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(y==0))),2)

# LOO balanced classification accuracy
round((mean(xor(ploo[y==0]>0.5,as.integer(y[y==0])))+
         mean(xor(ploo[y==1]<0.5,as.integer(y[y==1]))))/2,2)

qplot(pred, ploo)