Credit <- read.csv("~/Desktop/DANA-Qualt/Activity_4/Credit.dat", sep="")
attach(Credit)
View(Credit)

#a
n=sum(n)
model1 <- glm(cards/n ~income, family=binomial, weights=n, data = Credit)
summary(model1)

exp(0.10541)
install.packages('carData')
library(carData)
library(car)
Anova(model1)

confint(model1)

#b


logit_model <- glm(cards ~ ., data = Credit, family = "binomial")

# Get the coefficients
beta_0 <- coef(logit_model)[1]
beta_1 <- coef(logit_model)[2]

# Calculate the income value where log-odds are 0
income_value <- -beta_0 / beta_1

# Display the result
cat("The estimated logit value is 0 when income =", income_value, "thousand euros\n")

