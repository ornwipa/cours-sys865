# Exemple du TP5, 14 février 2024
data <- read.csv("diabetes.csv")
data_cleaned <- subset(data, Glucose != 0 &
                         BloodPressure != 0 &
                         SkinThickness != 0 &
                         Insulin != 0 & BMI != 0)

data0 <- subset(data_cleaned, Outcome==0)[1:8]
data1 <- subset(data_cleaned, Outcome==1)[1:8]

graphiqueQQ <- function(x) {
  qqnorm(x)
  qqline(x)
}

sapply(data0, graphiqueQQ)
sapply(data1, graphiqueQQ)

for (col in 1:ncol(data0)) {
  fig_name=paste("graphiqueQQ/GraphiqueQQ_", 
                 colnames(data)[col], "_Nondiabétique.jpg", 
                 sep = "")
  jpeg(file=fig_name)
  graphiqueQQ(data0[,col])
  dev.off()
}
for (col in 1:ncol(data1)) {
  fig_name=paste("graphiqueQQ/GraphiqueQQ_", 
                 colnames(data)[col], "_Diabétique.jpg", 
                 sep = "")
  jpeg(file=fig_name)
  graphiqueQQ(data1[,col])
  dev.off()
}

sapply(data0, shapiro.test)
sapply(data1, shapiro.test)

var.test(data0$BloodPressure, data1$BloodPressure)
# num df = 261, denom df = 129, p-value = 0.223
# 95 percent confidence interval:
#  0.6136085 1.1166999
# sample estimates:
# ratio of variances 
#          0.8341574 

#install.packages("car")
library(car)

leveneTest(SkinThickness ~ factor(Outcome), data = data_cleaned)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   1  2.1244 0.1458
#       390

leveneTest(Insulin ~ factor(Outcome), data = data_cleaned)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value  Pr(>F)  
# group   1  4.7765 0.02944 *
#       390