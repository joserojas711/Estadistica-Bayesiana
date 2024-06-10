### OPENBUGS ###
y ~ dbin(p, N)
p ~ dbeta(1, 1)

model <- function() { 
  # Prior 
  p ~ dbeta(1, 1) 
  
  # Likelihood 
  y ~ dbin(p, N) 
}


library(R2OpenBUGS) 
model.file <- file.path(tempdir(), 
                        +    "model.txt") 
write.model(model, model.file)

library(MASS) 
tbl <- table(survey$Smoke) 
N <- as.numeric(sum(tbl)); N 
y <- N - as.numeric(tbl["Never"]); y 

data <- list("N", "y")

params <- c("p")

inits <- function() { list(p=0.5) }

out <- bugs(data, inits, params, 
            +    model.file, n.iter=10000)

all(out$summary[,"Rhat"] < 1.1)
out$mean["p"] 
out$sd["p"] 
print(out, digits=5)

#MCMC Convergence

out <- bugs(data, inits, params, 
            +     model.file, codaPkg=TRUE, n.iter=10000)  out.coda <- read.bugs(out) 

library(coda) 
xyplot(out.coda)
densityplot(out.coda)
acfplot(out.coda)
gelman.diag(out.coda) 
gelman.plot(out.coda)
out.summary <- summary(out.coda, 
                       +     q=c(0.025, 0.975)) 
out.summary$stat["p",] 
out.summary$q["p", ]