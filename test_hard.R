library(mixR)
library(dplyr)
library(tidyverse)
require(dirichletprocess)
library(ggplot2)
library(data.table)
library(reshape2)
library(ggpubr)

# Generate some random data from a lognormal distribution mixture model.
# Fit a Dirichlet process type model to this simulated data. 
# Sample new data from the posterior of the final model and summarise the 5%/95% quantiles of the simulated data. 
# Explore how the prior distribution on the alpha parameter effects the number of clusters.
# Plot the alpha parameter chains after using different prior distributions to assess how long the model takes to converge.

poisMd <- MixingDistribution(distribution="poisson", priorParameters = c(1, 1), conjugate="conjugate")

Likelihood.poisson <- function(mdobj, x, theta){
    return(as.numeric(dpois(x, theta[[1]])))
}

PriorDraw.poisson <- function(mdobj, n){
    draws <- rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
    theta <- list(array(draws, dim=c(1,1,n)))
    return(theta)
}

PosteriorDraw.poisson <- function(mdobj, x, n=1){
    priorParameters <- mdobj$priorParameters
    lambda <- rgamma(n, priorParameters[1] + sum(x), priorParameters[2] + nrow(x))
    return(list(array(lambda, dim=c(1,1,n))))
}

Predictive.poisson <- function(mdobj, x){
    priorParameters <- mdobj$priorParameters
    pred <- numeric(length(x))
    for(i in seq_along(x)){
      alphaPost <- priorParameters[1] + x[i]
      betaPost <- priorParameters[2] + 1
      pred[i] <- (priorParameters[2] ^  priorParameters[1]) / gamma(priorParameters[1])
      pred[i] <- pred[i] * gamma(alphaPost) / (betaPost^alphaPost)
      pred[i] <- pred[i] * (1 / prod(factorial(x[i])))
    }
    return(pred)
}

data1 <- rpois(250, 4)
data2 <- rpois(250, 10)

# Set weights for each process
weight1 <- 0.8
weight2 <- 0.2

# Create a weighting vector based on the number of samples
weights <- c(rep(weight1, times = 250), rep(weight2, times = 250))

# Use the sample function with specified weights
y <- sample(c(data1, data2), size = 250 * 2, replace = TRUE, prob = weights)


# y <- c(rpois(250, 4), rpois(250, 10)) #generate sample data
dp <- DirichletProcessCreate(y, poisMd)
dp <- Initialise(dp)
dp <- Fit(dp, 2500)

data.frame(Weight=dp$weights, Theta=unlist(c(dp$clusterParameters)))


pf <- PosteriorFrame(dp, 0:20, 1000)

trueFrame <- data.frame(x=0:20,
                        y= 0.8*dpois(0:20, 4) + 0.2*dpois(0:20, 10))


ggplot() +
  geom_ribbon(data = pf, aes(x = x, ymin = X5., ymax = X95.), colour = NA, fill = "red", alpha = 0.2) +
  geom_line(data = pf, aes(x = x, y = Mean), colour = "red") +
  geom_line(data = trueFrame, aes(x = x, y = y)) +
  theme_minimal() -> poissonPlot

poissonPlot


