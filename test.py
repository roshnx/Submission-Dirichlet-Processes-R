import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import hdbscan

# Set seaborn style for better aesthetics
sns.set(style="whitegrid")

# Set random seed for reproducibility
np.random.seed(42)

# Function to generate data from a lognormal distribution mixture model
def generate_data(num_samples):
    proportions = [0.3, 0.7]  # Mixing proportions for the two lognormal distributions
    means = [0, 3]  # Means for the two lognormal distributions
    sigmas = [0.2, 0.5]  # Standard deviations for the two lognormal distributions

    # Generate random indices for each component based on proportions
    components = np.random.choice(len(proportions), size=num_samples, p=proportions)

    # Generate samples from lognormal distributions based on selected components
    samples = np.concatenate([np.random.lognormal(mean, sigma, size=np.sum(components == i))
                              for i, (mean, sigma) in enumerate(zip(means, sigmas))])

    return samples

# Generate simulated data
num_samples = 1000
simulated_data = generate_data(num_samples)

# Reshape the data to meet the requirements of hdbscan
simulated_data = simulated_data.reshape(-1, 1)

# Explore different prior distributions on the alpha parameter
alpha_values = [0.1, 1.0, 10.0, 1000.0, 100000.0]  # Different values for the concentration parameter alpha

plt.figure(figsize=(15, 5))

for i, alpha in enumerate(alpha_values):
    # Fit an HDBSCAN model with varying min_cluster_size (analogous to alpha)
    hdbscan_model = hdbscan.HDBSCAN(min_cluster_size=int(alpha), prediction_data=True)
    hdbscan_model.fit(simulated_data)

    # Get the number of clusters
    num_clusters = len(set(hdbscan_model.labels_)) - 1  # Exclude noise points (label -1)

    # Plot the results with KDE
    plt.subplot(1, len(alpha_values), i + 1)
    sns.kdeplot(simulated_data.flatten(), fill=True, color='lightgray', alpha=0.5)
    plt.scatter(simulated_data, np.zeros_like(simulated_data), c=hdbscan_model.labels_, cmap='viridis', s=20)
    plt.title(f'Min Cluster Size = {int(alpha)}\nClusters = {num_clusters}')
    plt.xlabel('Value')
    plt.ylabel('Density')

plt.tight_layout()
plt.show()
