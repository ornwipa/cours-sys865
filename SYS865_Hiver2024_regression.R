# TP du 13 mars 2024
data <- read.csv("diabetes.csv")
selected_columns <- data[, 2:6]
rows_with_zero <- apply(selected_columns, 1, function(x) any(x == 0))
data_cleaned <- data[!rows_with_zero, ]
summary(data_cleaned)

model_full <- lm(Outcome ~ Pregnancies + Glucose + 
                   BloodPressure + SkinThickness + Insulin + 
                   BMI + DiabetesPedigreeFunction + Age,
                 data = data_cleaned)
summary(model_full)

model_reduced <- lm(Outcome ~ Pregnancies + Glucose + 
                      BMI + DiabetesPedigreeFunction + Age,
                    data = data_cleaned)
summary(model_reduced)

plot(data_cleaned$Glucose, data_cleaned$Outcome, 
     main="Scatter Plot of Glucose vs Outcome",
     xlab="Glucose", ylab="Outcome", pch=19, col=rgb(0,0,1,0.5))

# abline(model_full, col="red")
abline(model_reduced, col="red")

linear_model <- lm(Outcome ~ Glucose, data=data_cleaned)
abline(linear_model, col="red")

# TP du 20 mars 2024
logistic_model <- glm(Outcome ~ Glucose, data=data_cleaned, family=binomial)

logistic_curve <- function(x) {
  predict(logistic_model, newdata=data.frame(Glucose=x), type="response")
}

curve(logistic_curve(x), add=TRUE, col="darkgreen")

logistic_model <- glm(Outcome ~ Pregnancies + Glucose + 
#                        BloodPressure + SkinThickness + Insulin + 
                        BMI + DiabetesPedigreeFunction + Age, 
                      data=data_cleaned, family=binomial)

mean_values <- colMeans(data_cleaned[, c("Pregnancies",
#                                        "BloodPressure", "SkinThickness", "Insulin", 
                                         "BMI", "DiabetesPedigreeFunction", "Age")])

logistic_prediction <- function(glucose) {
  predict(logistic_model, 
          newdata=as.data.frame(cbind(Pregnancies = mean_values["Pregnancies"], 
                                      Glucose = glucose, 
#                                      BloodPressure = mean_values["BloodPressure"], 
#                                      SkinThickness = mean_values["SkinThickness"], 
#                                      Insulin = mean_values["Insulin"], 
                                      BMI = mean_values["BMI"], 
                                      DiabetesPedigreeFunction = mean_values["DiabetesPedigreeFunction"], 
                                      Age = mean_values["Age"])), type="response")
}

plot(data_cleaned$Glucose, data_cleaned$Outcome, 
     main="Scatter Plot of Glucose vs Outcome",
     xlab="Glucose", ylab="Outcome", pch=19, col=rgb(0,0,1,0.5))

curve(logistic_prediction(x), add=TRUE, col="darkgreen", from=min(data_cleaned$Glucose), to=max(data_cleaned$Glucose))
