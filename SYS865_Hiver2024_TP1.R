# Exemple du TP1, 17 janvier 2024
data <- read.csv("diabetes.csv")
moyenne1 <- mean(data$Pregnancies)
variance1 <- var(data$Pregnancies)

hist(data$Pregnancies)
for (col in 1:ncol(data)) {
  fig_name=paste("histogramme/Histogramme", 
                 colnames(data)[col], ".jpg", 
                 sep = "")
  jpeg(file=fig_name)
  hist(data[,col], 
       main=paste("Histogramme du paramètre", 
                  colnames(data)[col]),  
       xlab=colnames(data)[col])
  dev.off()
}

covariances <- round(var(data),3)
correlations <- round(cor(data),3)

write.csv(covariances, "covariances.csv")
write.csv(correlations, "correlations.csv")
