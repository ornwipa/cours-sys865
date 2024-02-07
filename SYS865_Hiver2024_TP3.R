# Exemple du TP3, 24 janvier 2024
data <- read.csv("diabetes.csv") # 768 observations
data_cleaned <- subset(data, Glucose != 0 &
                         BloodPressure != 0 &
                         SkinThickness != 0 &
                         Insulin != 0 & BMI != 0) # 392 obs

data0 <- subset(data_cleaned, Outcome==0)
data1 <- subset(data_cleaned, Outcome==1)

length(data0$Outcome) # 262
length(data1$Outcome) # 130

var.test(data0$Pregnancies, data1$Pregnancies) # p-value = 4.23e-08
wilcox.test(data0$Pregnancies, data1$Pregnancies) # p-value = 7.53e-05
# Rejet H_0 : les nombres des "Pregnancies" ne sont pas égaux.

var.test(data0$BloodPressure, data1$BloodPressure) # p-value = 0.223
t.test(data0$BloodPressure, data1$BloodPressure, var.equal = TRUE) # p-value = 0.0001237
# Rejet H_0 : les pressions sanguins ne sont pas égales.

var.test(data0$SkinThickness, data1$SkinThickness) # p-value = 0.3133
t.test(data0$SkinThickness, data1$SkinThickness, var.equal = TRUE) # p-value = 2.793e-07
# Rejet H_0 : les épaisseurs de la peau ne sont pas égaux.

var.test(data0$Insulin, data1$Insulin) # p-value = 0.0005164
wilcox.test(data0$Insulin, data1$Insulin) # p-value = 1.216e-13
# Rejet H_0 : les taux d'insulin ne sont pas égaux.

var.test(data0$Age, data1$Age) # p-value = 0.02376
wilcox.test(data0$Age, data1$Age) # p-value = 3.944e-15
# Rejet H_0 : les âges ne sont pas égales.