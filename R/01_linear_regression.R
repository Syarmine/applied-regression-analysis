# 01_linear_regression

# Setup
knitr::opts_chunk$set(echo = TRUE)

# Load data
# setwd("C:~/applied-regression-analysis")
load(file.path("data",'hiedata_short.Rdata'))

# Look at the variable names in the data frame
colnames(hiedata_short)

# Summary of income variable
summary(hiedata_short$income)

# Summary of income and age
summary(hiedata_short[, c("income", "age")])

# Fitting a simple linear regression model
lm.mod <- lm(formula = income ~ age, 
             data = hiedata_short)

# Basic output
lm.mod

# Detailed output
summary(lm.mod)

# Calculate confidence intervals
confint(object = lm.mod,
        parm = c("x"),
        level = 0.95)

# At-Home Exercise 1
summary(hiedata_short[,c("income", "education", "age", "ghi")])

# At-Home Exercise 2
plot(x = hiedata_short$education,
     y = hiedata_short$income,
     type = "p",
     xlab = "Years of Education",
     ylab = "Family Income (1000s of USD)",
     main = "Income Versus Education\nRAND Health Insurance Experiment\n")

plot(x = hiedata_short$education,
     y = hiedata_short$income,
     type = "p",
     col = "black",
     xlab = "Years of Education",
     ylab = "Family Income (1000s of USD)",
     main = "Income Versus Education\nRAND Health Insurance Experiment\n")

lm.educ <- lm(formula = income ~ education,
              data = hiedata_short)

abline(reg = lm.educ,
       lwd = 2,
       col = "purple")

# At-Home Exercise 3
lm.educ <- lm(formula = income ~ education,
              data = hiedata_short)
summary(lm.educ)

# At-Home Exercise 4
lm.all <- lm(formula = income ~ education + age + ghi,
             data = hiedata_short)
summary(lm.all)

confint(object = lm.all,
        parm = c("education", "age", "ghi"),
        level = 0.95)

# At-Home Exercise 5
education.pts <- seq(from = 0,
                     to = 25,
                     by = 1)
print(education.pts)

fitted.values.data <- data.frame(age = 30,
                                 education = education.pts,
                                 ghi = 70)

head(fitted.values.data, 5)

fitted.values <- predict(object = lm.all,
                         newdata = fitted.values.data,
                         interval = "confidence",
                         level = 0.95)

fitted.values <- data.frame(fitted.values)

plot(x = education.pts,
     y = fitted.values$fit,
     type = "l",
     col = "black",
     xlab = "Years of Education",
     ylab = "Predicted Family Income (1000s of USD)",
     main = "Predicted Income Given Education\nRAND Health Insurance Experiment \n",
     sub = "Age fixed at 30; GHI fixed at 70",
     ylim = c(0, 20))

matlines(education.pts,
         fitted.values[, c("lwr", "upr")],
         lty = "dashed",
         col = "red",
         lwd = 2)

rug(x = jitter(hiedata_short$education, factor = 1, amount = NULL),
    ticksize = 0.03,
    side = 1,
    lwd = 0.5)

legend("topright",
       legend = c("95% CI"),
       col = c("red"),
       lty = 2,
       lwd = 2,
       bty = "n")

# In-Class Exercise 1
lm.all <- lm(formula = income ~ education + age + ghi,
             data = hiedata_short)
summary(lm.all)

confint(object = lm.all,
        parm = c("education", "age", "ghi"),
        level = 0.99)

# In-Class Exercise 2
summary(lm.educ)
summary(lm.all)

cor(hiedata_short[, c("income", "education", "age", "ghi")], 
    use = "complete.obs")