# TP du 17 janvier 2024
data <- read.csv("diabetes.csv")
expectations <- sapply(data, mean)
covariances <- var(data)
