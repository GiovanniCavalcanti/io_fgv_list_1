library(dplyr)
library(tidyr)
library(readxl)
library(ivreg)
library(gmm)

rm(list = ls())
gc()

# Load the dataset
dataset <- read_excel(path = "C:/Users/Giovanni Cavalcanti/OneDrive - Insper - Institudo de Ensino e Pesquisa/trimestre_6/fgv_io/list_01/io_fgv_list_1/ex2/dataset_ps1.xlsx") %>%
  mutate(market = as.factor(market),
         product = as.factor(product))

# Add the outside option
outside_option <- dataset %>%
  group_by(market) %>%
  summarize(share = 1 - sum(share), .groups = 'drop') %>%
  mutate(product = "outside",
         x1 = 0, x2 = 0, x3 = 0, x4 = 0, price = 0,
         iv1 = 0, iv2 = 0, iv3 = 0, iv4 = 0, iv5 = 0, iv6 = 0)

# Combine the original dataset with the outside option
dataset_with_outside <- bind_rows(dataset, outside_option) %>%
  mutate(product = as.factor(product))

# Fit the instrumental variable model
iv_model <- ivreg(
  log(share) - log(1 - ave(share, market, FUN = sum)) ~ x1 + x2 + x3 + x4 + price | 
    . - price + iv1 + iv2 + iv3 + iv4 + iv5 + iv6,
  data = dataset
)

# Load initial parameter guesses from iv_model coefficient
intercept <- iv_model$coefficients["(Intercept)"]
alpha <- iv_model$coefficients["price"]
beta <- iv_model$coefficients[c("x1", "x2", "x3", "x4")]
sigma <- 0.1  # Initial value for sigma

set.seed(123)  # For reproducibility

# Set up constants
num_consumers <- 500
tolerance <- 1e-6  # Set a convergence tolerance
max_iterations <- 10000  # Maximum number of iterations

# Generate consumer taste shocks
taste_shocks <- dataset_with_outside %>%
  distinct(market) %>%
  mutate(consumer_id = list(1:num_consumers)) %>%
  unnest(consumer_id) %>%
  mutate(v_i = rnorm(n()))

# Expand dataset with product-consumer-market combinations
expanded_dataset <- crossing(
  dataset_with_outside %>% select(market, product),
  consumer_id = unique(taste_shocks$consumer_id)
) %>%
  left_join(taste_shocks, by = c("market", "consumer_id")) %>%
  left_join(dataset_with_outside, by = c("market", "product"))

# Define the GMM function
gmm_fn <- function(params, data, max_iterations = 10000, tolerance = 1e-6) {
  
  # Extract parameters
  intercept <- params["intercept.(Intercept)"]
  alpha <- params["alpha.price"]
  beta <- params[grep("^beta", names(params))]
  sigma <- params["sigma"]
  
  # Initialize delta based on the current parameter values
  data <- data %>%
    mutate(delta = intercept + alpha * price + beta[1] * x1 + beta[2] * x2 + beta[3] * x3 + beta[4] * x4,
           mu_i = sigma * v_i * x4)
  
  # Main iteration loop to update delta
  for (iteration in 1:max_iterations) {
    
    # Calculate expected utility
    data <- data %>%
      mutate(exp_utility = exp(delta + mu_i)) %>%
      group_by(market, consumer_id) %>%
      mutate(total_exp_utility = sum(exp_utility)) %>%
      ungroup() %>%
      mutate(div = exp_utility / total_exp_utility) %>%
      group_by(market, product) %>%
      mutate(s_jm = sum(div) / num_consumers) %>%
      ungroup()
    
    # Calculate the new delta values
    data <- data %>%
      mutate(delta_new = delta + log(share) - log(s_jm))  # Update delta
    
    # Check for NA values in delta_new and delta
    if (any(is.na(data$delta_new))) {
      cat("NA detected in delta_new values. Exiting iteration.\n") 
      break
    }
    
    # Calculate the maximum difference for convergence check
    delta_diff <- max(abs(data$delta_new - data$delta), na.rm = TRUE)
    
    # Check for convergence
    if (delta_diff < tolerance) {
      cat("Converged in", iteration, "iterations.\n")
      cat("Converged with", params, "\n")
      break  # Exit the loop if converged
    }
    
    # Update delta for the next iteration
    data$delta <- data$delta_new
  }
  
  # Define the matrix of instruments (Z)
  Z <- as.matrix(data[, c("iv1", "iv2", "iv3", "iv4", "iv5", "iv6")])
  
  # Compute residuals based on the final delta
  X <- as.matrix(data[, c("price", "x1", "x2", "x3", "x4")])
  residuals <- data$delta - X %*% c(alpha, beta)
  
  # Calculate the moment conditions
  moments <- c(residuals) * Z
  
  # Return the average of the moments across observations
  return(colMeans(moments))
}


# Set up initial parameters for GMM estimation
initial_params <- c(intercept = intercept, alpha = alpha, beta = beta, sigma = sigma)

# Perform GMM estimation
gmm_results <- gmm(
  g = gmm_fn,
  x = expanded_dataset,
  t0 = initial_params,
  vcov = 'HAC',
  method = 'Nelder-Mead',
  control = list(reltol = 1e-25, maxit = 10000)
)

# Display results
coefs <- coef(gmm_results)

# Constants
alpha <- coefs['alpha.price'] 
beta <- coefs[3:6]
sigma <- coefs['sigma']   

# Filter data for market 1
market_data <- expanded_dataset %>%
  filter(market == 1, product != "outside") 

# Calculate individual choice probabilities s_ij for each product and consumer in market 1
market_data <- market_data %>%
  group_by(consumer_id) %>%
  mutate(
    delta = intercept + alpha * price + beta[1] * x1 + beta[2] * x2 + beta[3] * x3 + beta[4] * x4,
    mu = sigma * v_i * x4,
    exp_delta_mu = exp(delta + mu),
    total_exp_utility = sum(exp_delta_mu),
    s_ij = exp_delta_mu / total_exp_utility
  ) %>%
  ungroup()

# Calculate average market share for each product (s_j)
average_shares <- market_data %>%
  group_by(product) %>%
  summarize(s_j = mean(s_ij))

# Create a list of unique products in market one
products <- unique(market_data$product)

# Initialize an empty matrix to store elasticities
elasticity_matrix <- matrix(0, nrow = length(products), ncol = length(products),
                            dimnames = list(products, products))

# Calculate own- and cross-price elasticities
for (j in products) {
  # Filter data for product j
  product_data_j <- market_data %>% filter(product == j)
  s_j <- average_shares %>% filter(product == j) %>% pull(s_j)
  p_j <- product_data_j$price[1]  # Assuming price is constant across consumers for each product
  
  # Calculate own-price elasticity for product j
  elasticity_matrix[j, j] <- mean((p_j / s_j) * alpha * product_data_j$s_ij * (1 - product_data_j$s_ij))
  
  # Calculate cross-price elasticities for product j with respect to other products k
  for (k in products[products != j]) {
    product_data_k <- market_data %>% filter(product == k)
    p_k <- product_data_k$price[1]  # Price of product k
    elasticity_matrix[j, k] <- -mean((p_k / s_j) * alpha * product_data_j$s_ij * product_data_k$s_ij)
  }
}

# Display the elasticity matrix
print(elasticity_matrix)
