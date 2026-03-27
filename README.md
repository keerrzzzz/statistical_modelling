# statistical_modelling
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r windmill-analysis}
cat('1.The data in file windmill.txt were collected in a study to determine the effect of wind velocity on the output of a wind turbine. The data are 24 observations of wind velocity (in miles per hour) and direct current.')

windmill <- read.table("/Users/zhangkeer/Documents/r study/MATH2010/windmill .txt", header=TRUE)

cat('\n\ni)')

#current (y) verse velocity (x)
plot(windmill$velocity, windmill$current, 
     xlab="Velocity", ylab="Current", 
     main="Current vs. Velocity")
abline(lm(current ~ velocity, data = windmill), col = "red", lwd = 2)

cor_value <- cor(windmill$velocity, windmill$current)
cat('\nCorrelation:', round(cor_value, 4))
cat('\nsince the correlation is close to 1, it is strong linear')

#log-transformation both y and x
log_velocity <- log(windmill$velocity)
log_current <- log(windmill$current)

plot(log_velocity, log_current,
     xlab="log_Velocity", ylab = "log_Current",
     main="log(Current) vs. log(Velocity)")
abline(lm(log_current ~ log_velocity), col = "red", lwd = 2)

cat('\n log is a better linear relationship for modelling purposes since on the picture the points are more close to the line ')
```

```{r model-comparison}
cat("\n\nii)")

model_original <- lm(current ~ velocity, data = windmill)
model_log <- lm(log_current ~ log_velocity)

cat("\nORIGINAL SCALE MODEL")
print(summary(model_original))

cat("\nLOG-LOG SCALE MODEL")
print(summary(model_log))

r2_original <- summary(model_original)$r.squared
r2_log <- summary(model_log)$r.squared

cat("\nR² comparison:\n")
cat("Original scale:", round(r2_original, 4), "\n")
cat("Log-log scale: ", round(r2_log, 4), "\n")

par(mfrow = c(2, 2))

# Original scale residuals
plot(model_original$fitted.values, model_original$residuals,
     xlab = "Fitted", ylab = "Residuals",
     main = "Original: Residuals")
abline(h = 0, col = "red")

qqnorm(model_original$residuals, main = "Original: Q-Q Plot")
qqline(model_original$residuals, col = "red")

# Log-Log scale residuals
plot(model_log$fitted.values, model_log$residuals,
     xlab = "Fitted", ylab = "Residuals",
     main = "Log-Log: Residuals")
abline(h = 0, col = "red")

qqnorm(model_log$residuals, main = "Log-Log: Q-Q Plot")
qqline(model_log$residuals, col = "red")

cat("\nSince R² of log-log model (", round(r2_log, 4), ") is lower than original (", round(r2_original, 4), "), the original model shows better linear relationship.")
```
```{r model-analysis}
cat('iii) choose original model\n')
preferred_model <- model_original
cat('estimates\n')
coef_1 <- summary(preferred_model)$coefficients
print(round(coef_1, 4))
cat('95% confidence interval\n')
ci <- confint(preferred_model, level = 0.95)
print(round(ci, 4))
cat('error variance\n')
sigma <- summary(preferred_model)$sigma
sigma2 <- sigma^2

cat("error variance σ² =", round(sigma2, 4), "\n")
```
```{r supervisors-analysis}
cat("\n\n========================================\n")
cat('The number of supervisors and the number of supervised workers in 27 industrial establishments of varying size were recorded. We want to model the relationship between the number of supervisors (the response) and the number of supervised workers (the explanatory variable). The data are available in super.txt.')

super <- read.table('/Users/zhangkeer/Documents/r study/MATH2010/super.txt', header=TRUE)

cat('\n\ni)')

log_workers <- log(super$Workers)
log_supervisors <- log(super$Supervisors)
ratio <- super$Supervisors / super$Workers
neg_reciprocal <- -1 / super$Workers

# Set up for three plots
par(mfrow = c(1, 3))

# a) Original scale
plot(super$Workers, super$Supervisors,
     xlab = "Workers", ylab = "Supervisors",
     main = "Original Scale")
abline(lm(Supervisors ~ Workers, data = super), col="red")

# b) Log-Log scale
plot(log_workers, log_supervisors,
     xlab = "log_Workers", ylab = "log_Supervisors",
     main = "Log-Log Scale")
abline(lm(log_supervisors ~ log_workers), col="red")

# c) Ratio scale
plot(neg_reciprocal, ratio,
     xlab = "-1/Workers", ylab = "Supervisors/Workers",
     main = "Ratio Scale")
abline(lm(ratio ~ neg_reciprocal), col="red")

# Reset plot parameters
par(mfrow = c(1, 1))

cor_original <- cor(super$Workers, super$Supervisors)
cor_log <- cor(log_workers, log_supervisors)
cor_ratio <- cor(neg_reciprocal, ratio)

cat("\nCorrelations:")
cat("\nOriginal scale:", round(cor_original, 4))
cat("\nLog-log scale:", round(cor_log, 4))
cat("\nRatio scale:", round(cor_ratio, 4))
cat('\nThe log scale shows the best linear relationship because the points follow a straight line most closely and the correlation is highest')
```
```{r simple-linear-regression-model}
cat('\n\nii)')

model_log2 <- lm(log_supervisors ~ log_workers)

cat("\nLOG-LOG SCALE MODEL")
print(summary(model_log2))

r2_log <- summary(model_log2)$r.squared

cat("\nLog-log scale:", round(r2_log, 4))

# Residual plots
par(mfrow = c(1,1))

plot(model_log2$fitted.values, model_log2$residuals,
     xlab = "Fitted", ylab = "Residuals",
     main = "Log-Log: Residuals")
abline(h = 0, col = "red")

qqnorm(model_log2$residuals, main = "Log-Log: Q-Q Plot")
qqline(model_log2$residuals, col = "red")
cat('This model is fit to the data. Residuals randomly scattered around zero. And in Q-Q plot, most points follow the line closely.')
```
```{r supervisors-quadratic}
cat('\n\niii) choose log-log based on ii)')

log_workers_sq <- log_workers^2
model_quad2 <- lm(log_supervisors ~ log_workers + log_workers_sq)
model_simple2 <- lm(log_supervisors ~ log_workers)

cat("\nQUADRATIC MODEL TEST\n")
print(summary(model_quad2))

anova_result2 <- anova(model_simple2, model_quad2)
print(anova_result2)

p_quad <- summary(model_quad2)$coefficients[3,4]
cat("\nP-value for quadratic term:", round(p_quad, 4))

cat('\nSince p_quad=0.1415, greater than 0.05, thus simple linear model is sufficient')
final_model <- model_simple2

```
```{r final-model-details}
cat('\n\niv)')

cat("FITTED MODEL (log-log scale)\n")

final_model<-model_simple2
coef_table <- summary(final_model)$coefficients
intercept <- -1.48458
slope <- 0.90920
a <- exp(intercept)
cat("Supervisors =", round(a, 4), "× Workers^", round(slope, 4), "\n")

ci_slope1 <- confint(final_model, "log_workers", level = 0.95)

cat("\n95% CI for log_workers:", cat(round(ci_slope1, 4)))

sigma <- summary(final_model)$sigma
sigma2 <- sigma^2
df <- df.residual(final_model)

cat("\n\nResidual standard error (sigma):", round(sigma, 4))
cat("\nResidual variance (sigma²):", round(sigma2, 4))
cat("\nDegrees of freedom:", df)
```
```{r prediction}
cat('\n\nv)')

final_model <- lm(log(Supervisors) ~ log(Workers), data = super)
cat("Supervisors =", round(exp(coef(final_model)[1]), 4), 
    "× Workers^", round(coef(final_model)[2], 4), "\n")


new <- data.frame(Workers = 1600)
pred_log <- predict(final_model, newdata = new, interval = "confidence",level = 0.95)
pred_orig <- exp(pred_log)



cat("\nPrediction for 1600 workers:\n")
cat("Predicted supervisors:", round(pred_orig[1], 4), "\n")
cat("95% CI:", round(pred_orig[2], 4), "to", round(pred_orig[3], 4), "\n")

cat('since 256 is greater than 213.4052, so it is outside, thus would be surpising')



```
