# Exemple du TP2, 24 janvier 2024
calculate_ci <- function(x, conf_level = 0.95) {
  mu <- mean(x, na.rm = TRUE)
  stddev <- sd(x, na.rm = TRUE)
  n <- sum(!is.na(x))
  alpha <- 1 - conf_level
  se <- stddev / sqrt(n)
  t_critical <- qt(1 - alpha/2, df = n - 1)
  margin_error <- t_critical * se
  ci_lower <- mu - margin_error
  ci_upper <- mu + margin_error
  return(c(lower = ci_lower, upper = ci_upper))
}

data <- read.csv("diabetes.csv") # 768 observations
data_cleaned <- subset(data, Glucose != 0 &
                BloodPressure != 0 &
                SkinThickness != 0 &
                Insulin != 0 & BMI != 0) # 392 obs

data0 <- subset(data_cleaned, Outcome==0)
data1 <- subset(data_cleaned, Outcome==1)

calculate_ci(data_cleaned$Pregnancies)
#    lower    upper 
# 2.982124 3.619916 
calculate_ci(data0$Pregnancies)
#    lower    upper 
# 2.402911 3.039838
calculate_ci(data1$Pregnancies)
#    lower    upper 
# 3.789668 5.148793

sapply(data_cleaned[,1:8], calculate_ci)
#       Pregnancies  Glucose BloodPressure SkinThickness  Insulin      BMI DiabetesPedigreeFunction      Age
# lower    2.982124 119.5631      69.42240      28.10112 144.2551 32.38837                0.4887388 29.85185
# upper    3.619916 125.6920      71.90413      30.18969 167.8572 33.78407                0.5573531 31.87774
sapply(data0[,1:8], calculate_ci)
#       Pregnancies  Glucose BloodPressure SkinThickness  Insulin      BMI DiabetesPedigreeFunction      Age
# lower    2.402911 108.4336      67.52269      25.98258 118.3704 30.92415                0.4357651 27.25381
# upper    3.039838 114.4290      70.41624      28.52123 143.3395 32.57738                0.5085708 29.44085
sapply(data1[,1:8], calculate_ci)
#       Pregnancies  Glucose BloodPressure SkinThickness  Insulin      BMI DiabetesPedigreeFunction      Age
# lower    3.789668 140.0143      71.81732      31.28825 183.8190 34.60904                0.5551479 34.09304
# upper    5.148793 150.3703      76.33652      34.63483 229.8733 36.94635                0.6960214 37.7838

plot_normal_with_ci <- function(x, x0, x1) {
  calculate_stats <- function(data) {
    mean_data <- mean(data)
    sd_data <- sd(data)
    ci <- calculate_ci(data)
    return(list(mean = mean_data, sd = sd_data, lower_ci = ci[1], upper_ci = ci[2]))
  }
  stats_x <- calculate_stats(x)
  stats_x0 <- calculate_stats(x0)
  stats_x1 <- calculate_stats(x1)
  datasets <- list(stats_x, stats_x0, stats_x1)
  colors <- c("blue", "black", "red")
  xlim <- range(c(sapply(datasets, function(g) g$mean - 4 * g$sd), 
                  sapply(datasets, function(g) g$mean + 4 * g$sd)))
  ylim <- c(0, max(sapply(datasets, function(g) dnorm(g$mean, g$mean, g$sd))))
  plot(1, type = "n", xlim = xlim, ylim = ylim, 
       xlab = "Value", ylab = "Density", 
       main = "Normal Distributions with Confidence Intervals")
  for (i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    x_seq <- seq(dataset$mean- 4*dataset$sd, dataset$mean+4*dataset$sd, length.out = 100)
    y <- dnorm(x_seq, dataset$mean, dataset$sd)
    lines(x_seq, y, col = colors[i], lwd = 2)
    abline(v = dataset$mean, col = colors[i], lwd = 2, lty = 2)
    abline(v = dataset$lower_ci, col = colors[i], lwd = 2, lty = 3)
    abline(v = dataset$upper_ci, col = colors[i], lwd = 2, lty = 3)
  }
}
plot_normal_with_ci(data_cleaned$Pregnancies, data0$Pregnancies, data1$Pregnancies)
