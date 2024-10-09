# Install and load the required packages
# Uncomment the following lines if you haven't installed these packages yet
# install.packages("brms")
# install.packages("ggplot2")

library(brms)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Step 1: Simulate data for 107 guests using the Negative Binomial distribution

# Define parameters for the Negative Binomial distribution
size_param <- 5    # Dispersion parameter (size, or 'shape')
mean_param <- 4    # Mean parameter (mu)

# Simulate data for 107 guests
consumption_data <- rnbinom(107, size = size_param, mu = mean_param)

# Create a data frame for modeling
data <- data.frame(Consumption = consumption_data)

# Step 2: Fit a Bayesian Negative Binomial Model using brms

# Define the model formula
formula <- bf(Consumption ~ 1)

# Fit Bayesian Negative Binomial model using brms
nb_model <- brm(
  formula = formula, 
  data = data, 
  family = negbinomial(),  # Specifies the Negative Binomial family
  prior = set_prior("normal(0, 10)", class = "Intercept"), # Set prior only for the intercept
  chains = 4, 
  iter = 2000, 
  seed = 123
)


# Step 3: Check the model summary and diagnostics
summary(nb_model)
plot(nb_model)

# Step 4: Visualize the posterior predictive distribution

# Generate posterior predictive samples
posterior_predictive <- posterior_predict(nb_model)

total_consumption <- rowSums(posterior_predictive)

# Plot histogram of posterior predictive samples
ggplot(data.frame(Consumption = as.vector(posterior_predictive)), aes(x = Consumption)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Posterior Predictive Distribution of Alcohol Consumption",
       x = "Units of Alcohol Consumed", y = "Frequency") +
  theme_minimal()

total_consumption <- rowSums(posterior_predictive)

ggplot(data.frame(Consumption = as.vector(total_consumption)), aes(x = Consumption)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Antal enheter",
       x = "Antal enheter under kvällen", y = "Antal simuleringar") +
  theme_minimal()

# Step 5: Compute the 95% credible interval for total consumption
credible_interval <- quantile(total_consumption, probs = c(0.025, 0.975))

num_guests_one_drink <- apply(posterior_predictive, 1, function(x) sum(x == 1))

# Display the credible interval
print(credible_interval)

bayestestR::hdi(num_guests_one_drink, ci = 0.89)
2

install.packages("easystats")
