# Exemple du TP6, 21 février 2024
data <- read.csv("diabetes.csv")
data <- subset(data, Glucose != 0)
data0 <- subset(data, Outcome==0)[1:6,1:8]
data1 <- subset(data, Outcome==1)[1:3,1:8]

mean(data0$Glucose) # 109
mean(data1$Glucose) # 156

sd(data$Glucose) # 30.53564
sd_pool <- sqrt((5*var(data0$Glucose) + 2*var(data1$Glucose))/7)
sd_pool # 21.09841

effect_size <- (mean(data0$Glucose) - mean(data1$Glucose))/sd_pool
effect_size # -2.227656

ncp <- effect_size*sqrt((5*2)/(5+2))
ncp # -2.662558

df <- 9-2
t_critical <- qt(0.975, df)
t_critical # -2.662558

power <- pt(t_critical, df, ncp)
power # 2.364624

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
calculate_sample_size(effect_size, 0.8, 0.05) # 3
