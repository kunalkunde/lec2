compute_portfolio_volatility <- function(weights, volatilities, correlation_matrix) {
  # Ensure the inputs are numeric vectors/matrices
  weights <- as.numeric(weights)
  volatilities <- as.numeric(volatilities)
  correlation_matrix <- as.matrix(correlation_matrix)
  
  # Check for dimension consistency
  n <- length(weights)
  if (length(volatilities) != n || nrow(correlation_matrix) != n || ncol(correlation_matrix) != n) {
    stop("Dimensions of weights, volatilities, and correlation matrix must match.")
  }
  
  # Create the diagonal matrix of volatilities
  volatility_matrix <- diag(volatilities)
  
  # Compute the covariance matrix
  covariance_matrix <- volatility_matrix %*% correlation_matrix %*% volatility_matrix
  
  # Compute the portfolio variance
  portfolio_variance <- t(weights) %*% covariance_matrix %*% weights
  
  # Compute and return the portfolio volatility
  portfolio_volatility <- sqrt(portfolio_variance)
  return(as.numeric(portfolio_volatility))
}

# Example inputs
weights <- c(0.3, 0.4, 0.3)  # Portfolio weights
volatilities <- c(0.08, 0.12, 0.20)  # Asset volatilities
correlation_matrix <- matrix(
  c(1.0, 0.3, 0.4,
    0.3, 1.0, 0.5,
    0.4, 0.5, 1.0),
  nrow = 3, byrow = TRUE
)  # Correlation matrix

# Compute portfolio volatility
portfolio_volatility <- compute_portfolio_volatility(weights, volatilities, correlation_matrix)
print(portfolio_volatility)