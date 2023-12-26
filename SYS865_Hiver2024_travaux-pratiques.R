# TP du 17 janvier 2024
data <- read.csv("diabetes.csv")
expectations <- sapply(data, mean)
covariances <- var(data)

# TP du 24 janvier 2024
# Filter out 0 values from the Glucose column
filtered_data <- subset(data, Glucose > 0)

# Define a function to calculate the confidence interval
calculate_ci <- function(data, conf_level = 0.95) {
  mean_glucose <- mean(data$Glucose, na.rm = TRUE)
  sd_glucose <- sd(data$Glucose, na.rm = TRUE)
  n <- sum(!is.na(data$Glucose))
  
  alpha <- 1 - conf_level
  se <- sd_glucose / sqrt(n)
  t_critical <- qt(1 - alpha/2, df = n - 1)
  margin_error <- t_critical * se
  
  ci_lower <- mean_glucose - margin_error
  ci_upper <- mean_glucose + margin_error
  
  return(c(lower = ci_lower, upper = ci_upper))
}

# Calculate CI for filtered data and each Outcome group
ci_all <- calculate_ci(filtered_data)
ci_outcome_0 <- calculate_ci(subset(filtered_data, Outcome == 0))
ci_outcome_1 <- calculate_ci(subset(filtered_data, Outcome == 1))

# Calculate density for each group
density_all <- density(filtered_data$Glucose, na.rm = TRUE)
density_outcome_0 <- density(subset(filtered_data, Outcome == 0)$Glucose, na.rm = TRUE)
density_outcome_1 <- density(subset(filtered_data, Outcome == 1)$Glucose, na.rm = TRUE)

# Find the range for the plot
x_lim_range <- range(density_all$x, density_outcome_0$x, density_outcome_1$x)
y_lim_range <- range(density_all$y, density_outcome_0$y, density_outcome_1$y)

# Set up the plot with appropriate limits
plot(density_all, xlim = x_lim_range, ylim = y_lim_range, 
     main = "PDF of plasma glucose parameter", 
     xlab = "", ylab = "Probability density", col = "black")

# Overlay the density plots for each group
lines(density_outcome_0, col = "blue")
lines(density_outcome_1, col = "red")

# Add a legend
legend("topright", legend = c("Tous", "Outcome 0", "Outcome 1"), 
       col = c("black", "blue", "red"), lty = 1, cex = 0.8)
