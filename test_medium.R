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
# Sample new data from the posterior of the final model and summarise the 5%/95% quantiles of the simulated data.Explore how the prior distribution on the alpha parameter effects the number of clusters.
# Plot the alpha parameter chains after using different prior distributions to assess how long the model takes to converge.

set.seed(124)
data <- rmixlnorm(n = 200, pi=c(0.8,0.2), mu = c(1,5), sd=c(0.5,0.6))
# data <- rmixlnorm(n = 200, pi=c(1), mu = c(1), sd=c(1))
# Create a density plot
plot(density(data), main = "Density Plot of Mixture of Log-Normal Distributions", 
     xlab = "Values", col = "blue", lwd = 2)

data = scale(data)
plot(density(data), main = "Density Plot of Mixture of Scaled Log-Normal Distributions", 
     xlab = "Values", col = "blue", lwd = 2)


hist(data, breaks = 30, col = "lightblue", xlab = "x", main = "Lognormal mixture model")


alpha_priors_list <- list(c(1, 1), c(2, 1), c(3, 1), c(4, 1), c(5, 1))

# Create an empty list to store the results
dp_list <- list()

# Fit the models in a loop
for (i in seq_along(alpha_priors_list)) {
  dp_list[[i]] <- Fit(DirichletProcessGaussian(data, alphaPriors = alpha_priors_list[[i]]), 1000)
}


xGrid <- seq(-2, 3, by=0.01)

postSamples <- data.frame(replicate(1000, PosteriorFunction(dp_list[[1]])(xGrid)))

postFrame <- data.frame(x=xGrid, y=rowMeans(postSamples))

quantileFrame <- data.frame(x = xGrid, t(apply(postSamples, 1, quantile, prob = c(0.05, 0.95))))


percentiles <- quantile(postFrame$y, c(0.05, 0.95))

# View the percentiles
print(percentiles)


ggplot(data = postFrame, aes(x = x, y = y)) +
  geom_line(color = 'red') +
  labs(title = "Posterior Density Plot",
       x = "xGrid",
       y = "Average Posterior Density")


# Alpha parameter and the number of clusters
alpha_priors_first <- sapply(alpha_priors_list, function(x) x[1])

# Create a data frame for the plot
plot_data <- data.frame(
  AlphaPrior = alpha_priors_first,
  NumberClusters = sapply(dp_list, function(dp) dp$numberClusters)
)

# Plot the data
plot_alpha_noClusters <- ggplot(plot_data, aes(x = AlphaPrior, y = NumberClusters)) +
  geom_point() +
  labs(title = "Number of Clusters vs. Alpha Prior (First Value)", x = "Alpha Prior (First Value)", y = "Number of Clusters")

# ggsave("plots/medium/Plot_alpha_noClusters.png", plot = plot_alpha_noClusters, width = 6, height = 4, units = "in")



# Create a data frame with alpha chains
alphaFrame <- data.frame(Iter = seq_len(1000))


# Add columns for each chain
for (i in seq_along(dp_list)) {
  alphaFrame[[paste0("Chain", i)]] <- dp_list[[i]]$alphaChain
}

# Reshape the data frame
alphaFrameTidy <- melt(alphaFrame, id.vars = "Iter")

beta_labels <- sapply(alpha_priors_list, function(p) {
  paste("Beta(", paste(p, collapse = ","), ")", sep = "")
})

# Assuming alphaFrameTidy has columns Iter, value, and variable 
plot_alphachain <-  ggplot(alphaFrameTidy, aes(x = Iter, y = value, colour = variable)) +
  geom_line() +
  labs(
    title = "AlphaChain values for 5 models",
    x = "Iteration",
    y = "AlphaChain Value",
    colour = "Beta Distribution Parameters"
  ) +
  scale_colour_manual(values = c("red", "blue", "green", "purple", "orange"),
                      labels = beta_labels)

plot_alphachain
# ggsave("plots/medium/Plot_alphachain.png", plot = plot_alphachain, width = 6, height = 4, units = "in")
