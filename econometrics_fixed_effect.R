# Load necessary libraries
library(plm)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate panel data
n <- 100  # number of individuals
t <- 10   # number of time periods

# Individual and time identifiers
id <- rep(1:n, each = t)
time <- rep(1:t, times = n)

# Simulate independent variables
x1 <- rnorm(n * t)
x2 <- rnorm(n * t)

# Simulate individual-specific effects
alpha <- rep(rnorm(n), each = t)

# Simulate time-specific effects
gamma <- rep(rnorm(t), times = n)

# Simulate error term
epsilon <- rnorm(n * t)

# Simulate dependent variable
y <- 5 + 2 * x1 + 3 * x2 + alpha + gamma + epsilon

# Create a data frame
panel_data <- data.frame(id, time, y, x1, x2)

# Convert to pdata.frame
pdata <- pdata.frame(panel_data, index = c("id", "time"))

# Display the first few rows
head(pdata)


# example data

# Load the Produc dataset from the plm package
data("Produc", package = "plm")

# Display the first few rows of the dataset
head(Produc)

# Convert to a panel data frame
pdata <- pdata.frame(Produc, index = c("state", "year"))

# Display the first few rows of the pdata
head(pdata)

# Fit a fixed effects model on the Produc dataset
fixed_effects_model <- plm(gsp ~ pcap + hwy + water + util, data = pdata, model = "within")

# Summarize the model
summary(fixed_effects_model)
