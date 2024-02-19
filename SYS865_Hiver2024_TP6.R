# Exemple du TP6, 21 février 2024
data <- read.csv("diabetes.csv")
data <- subset(data, Glucose != 0)
data0 <- subset(data, Outcome==0)[1:5,1:8]
data1 <- subset(data, Outcome==1)[1:5,1:8]

mean(data0$Glucose) # 103
mean(data1$Glucose) # 148.6

sd(data$Glucose) # 30.53564
sd_pool <- sqrt((4*var(data0$Glucose) + 4*var(data1$Glucose))/8)
sd_pool # 334.50942

effect_size <- (mean(data0$Glucose) - mean(data1$Glucose))/sd_pool
effect_size # -1.321378

ncp <- effect_size*sqrt((5*5)/(5+5))
ncp # -2.089283

df <- 10-2
t_critical <- qt(0.975, df)
t_critical # 2.306004

power <- pt(t_critical, df, ncp)
power # 0.9999373

calculate_sample_size <- function(d, power, alpha) {
  n <- 2 # petit n sera incrémenté
  while(TRUE) {
    t_crit <- qt(1 - alpha/2, df = 2*n - 2)
    ncp <- sqrt(n) * d
    beta <- pt(t_crit, df = 2*n - 2, ncp = ncp) -
      pt(-t_crit, df = 2*n - 2, ncp = ncp)
    current_power <- 1 - beta
    if (current_power >= power) {
      return(n)
    }
    n <- n + 1
  }
}
calculate_sample_size(effect_size, 0.8, 0.05) # 6
