library(datasets)
library(dirichletprocess)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
data("penguins")
data(iris)

options(device = "windows")  

# Easy Download the package, fit the normal mixture model to the faithful data set. 
# Fit the multivariate normal model on the iris or palmerspenguin dataset. 
# Plot the resulting distribution for both models.

# Fit the normal mixture model to the faithful data set
data <- faithful
# Plot the dataset
scatter_plot_faithful <- ggplot(data, aes(x=waiting, y=eruptions)) + geom_point() +
    labs(title = "Scatter Plot of 'faithful' Dataset",
       x = "Waiting",
       y = "Eruptions")

# ggsave("plots/easy/scatter_plot_faithful.png", plot = scatter_plot_faithful, width = 6, height = 4)

faithfulTrans_waiting <- (faithful$waiting - mean(faithful$waiting)) / sd(faithful$waiting)
faithfulTrans_eruptions <- (faithful$eruptions - mean(faithful$eruptions)) / sd(faithful$eruptions)
faithfulTrans <- c(faithfulTrans_waiting, faithfulTrans_eruptions)

faithfulTrans_df <- data.frame(waiting = faithfulTrans_waiting, eruptions = faithfulTrans_eruptions)

scatter_plot_scaled_faithful <- ggplot(data = faithfulTrans_df, aes(x = waiting, y = eruptions)) +
  geom_point() +
  labs(title = "Scatter Plot of Scaled faithful Dataset",
       x = "Waiting",
       y = "Eruptions")
       
# ggsave("plots/easy/scatter_plot_scaled_faithful.png", plot = scatter_plot_scaled_faithful, width = 6, height = 4)


# Fit to mixture of normal distributions 
dp <- DirichletProcessGaussian(faithfulTrans)
dp <- Fit(dp, 1500)

# Plot Dirichlet Process Gaussian
fit_density_plot_faithful <- plot(dp)

ggsave("plots/easy/Fit_density_plot_faithful.png", plot = fit_density_plot_faithful, width = 6, height = 4)


# The weight of each cluster with mean and Std.dev
data.frame(Weights=dp$weights,
                  mu=c(dp$clusterParameters[[1]]),
                  sigma=c(dp$clusterParameters[[2]]))

# Plot histograms and posterior function
xGrid <- seq(-3, 3, by=0.01)
postSamples <- data.frame(replicate(100, PosteriorFunction(dp)(xGrid)))

postFrame <- data.frame(x=xGrid, y=rowMeans(postSamples))

p_gaussian_mixture <- ggplot() + geom_histogram(data=data.frame(x=faithfulTrans), aes(x=x, y=..density..), binwidth = 0.25) + 
        geom_line(data=postFrame, aes(x=x,y=y), colour='red')

# Display the ggplot object
plot(p_gaussian_mixture)

# ggsave("plots/easy/Fit_histogram_plot_faithful.png", plot = p_gaussian_mixture, width = 6, height = 4)


# Fit the multivariate normal model on the iris or palmerspenguin dataset. 
# Plot the resulting distribution for both models.
irisPred <- iris[, -5]
irisPred <- scale(irisPred)
colnames(irisPred)

iris %>% select(-Species) %>% scale -> irisPred 
dp <- DirichletProcessMvnormal(irisPred)
dp <- Fit(dp, 5000, progressBar = TRUE)


# png("plots/easy/Fit_clusters_iris.png", width = 800, height = 600)
fit_clusters_iris <- pairs(irisPred, col = dp$clusterLabels)
# dev.off()

pairs(irisPred, col = dp$clusterLabels)

