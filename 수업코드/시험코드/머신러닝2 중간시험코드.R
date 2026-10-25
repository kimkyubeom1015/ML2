library(ggplot2)
library(psych)
install.packages("rstatix")
install.packages("skimr")
library(rstatix)
library(skimr)
library(lmtest)
library(car)
library("MASS")







train_data <- read.csv("C:/Users/user/Downloads/단순회귀분석_데이터셋/01.simple_train_data.csv")

ggplot(train_data, aes(x= thigh, y= upper_arm))+
  geom_point(alpha =0.7)+
  labs(title = "대퇴부 vs 상박부 삼전도",
       x= "대퇴부 둘레(thigh,cm)",
       y= "상박부 둘레 (upper)arm, cm)")+
  theme_minimal()

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)

train_data <- read.csv("C:/Users/USER/Downloads/단순회귀분석_데이터셋/01.simple_train_2.csv")

ggplot(train_data, aes(x= thigh, y= upper_arm))+
  geom_point(alpha =0.7)+
  labs(title = "대퇴부 vs 상박부 삼전도",
       x= "대퇴부 둘레(thigh,cm)",
       y= "상박부 둘레 (upper)arm, cm)")+
  theme_minimal()


model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)





data <- read.csv("C:/Users/USER/Downloads/Automobile (1).csv")

ggplot(data, aes(x= mpg, y= weight))+
  geom_point(alpha =0.7)+
  labs(title = "연비 vs 무게",
       x= "연비",
       y= "무게")+
  theme_minimal()

model <- lm(weight ~ mpg, data = data)
summary(model)





heights <- c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights <- c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p 
result_pearson$r 




train_data <- read.csv("C:/Users/USER/Downloads/단순회귀분석_데이터셋/01.simple_train_data.csv")
test_data <- read.csv("C:/Users/USER/Downloads/단순회귀분석_데이터셋/01.simple_test_data.csv")

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

predicted <- predict(loaded_model, newdata = test_data)
head(predicted)

results <- data.frame(Actual = test_data$upper_arm, Predicted = predicted)

rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:",round(rmse,3)))




df <- read.csv("C:/Users/USER/Downloads/02.다중회귀분석_실습/02.diabetes.csv"
               ,header = TRUE, stringsAsFactors = TRUE)
skim(df)


df <- na.omit(df)


df <- select(df, -c(Outcome))
skim(df)


result <- lm(df$Diabetes ~ . , data = df) 
result1 <- lm(df$Diabetes ~ 1, data = df)


anova_result <- anova(result1,result)
print(anova_result)


summary(result)

df<-select(df,-c(Pregnancies,BloodPressure,BMI,Age))


result <- lm(df$Diabetes ~ . , data = df) 
summary(result)








Fitted <- fitted(result) 
Residuals <- resid(result)

ggplot(df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted 선",
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()





StdResid <- sqrt(abs(df$Residuals)) 
ggplot(df, aes(x = Fitted, y = StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot 등",
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
  theme_minimal()

bptest(result)





durbinWatsonTest(result)




shapiro_test(result$residuals)



opar <- par(no.readonly = TRUE) 
par(mfrow = c(2, 2)) 
plot(result) 
par(opar) 





vif_values <- vif(result)
print(vif_values)



selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)




result_bk = lm(df$Diabetes~.,data= df)
DF_fit_bk =stepAIC(result_bk, direction = "backward", trace =T) 


result_fw = lm(df$Diabetes ~ 1, data= df)
DF_fit_fk = stepAIC(result_fw,direction ="forward",scope = (
  df$Diabetes ~ Pregnancies + Glucose + SkinThickness + Insulin + 
    BMI + Age),trace=T)




cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result) )
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf) 
cbind(round(cand, 2), z[rownames(cand), ])


out_rows <- c("229","446")
df2 <- df[!rownames(df) %in% out_rows, ]






result2 <- lm(Diabetes ~ . , data= df2)
cand <- influencePlot(result2, id = list(n=3))


max(cooks.distance(result2))







result2 <- lm(df$Diabetes ~ . , data = df) 
summary(result)

saveRDS(result2, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

   new_data <- read.csv("C:/Users/USER/Downloads/02.다중회귀분석_실습/02.diabetes_test.csv")

   predicted <- predict(loaded_model , newdata = new_data)
   head(predicted)

   results <- data.frame(new_data, Predicted = predicted)

rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse,3)))


































install.packages("rstatix")
install.packages("skimr")
library(rstatix)
library(skimr)

df <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes.csv"
               ,header = TRUE, stringsAsFactors = TRUE)
skim(df)


df <- na.omit(df)


df <- select(df, -c(Outcome))
skim(df)

result <- lm(df$Diabetes ~ . , data = df)
result1 <- lm(df$Diabetes ~ 1, data = df)


anova_result <- anova(result1,result)
print(anova_result)

summary(result)

df<-select(df,-c(Pregnancies,BloodPressure,BMI,Age))


result <- lm(df$Diabetes ~ . , data = df) 
result1 <- lm(df$Diabetes ~ 1, data = df)

summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes_test.csv")

predicted <- predict(loaded_model , newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)

rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse,3)))
