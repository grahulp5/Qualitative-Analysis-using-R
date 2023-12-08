Analgesic <- read.csv("~/Desktop/DANA-Qualt/activity_1/Analgesic.csv", sep="")

attach(Analgesic)

install.packages('dplyr')
library(dplyr)

y<-sum(New_Analgesic=='Yes')
y
n<-length(New_Analgesic)
n
phat<-y/n
phat

SE=sqrt((phat*(1-phat))/n)
SE

z=(phat-0.5)/SE
z

pvalue1=2*pnorm(z,lower.tail = FALSE)
pvalue1

z^2

pvalue2=pchisq(z^2,1,lower.tail = FALSE)
pvalue2

ztest=prop.test(y,n,p=0.5,alternative = "two.sided", correct = FALSE,conf.level = 0.95)
ztest

#likelihoood

likelihood_test <- function(phat, pnull, y, n) {
  LL <- list(
    'LRTstatistic' = (2 * (log(dbinom(x = y, size = n, prob = phat) / dbinom(x = y, size = n, prob = pnull)))),
    'p-value' = pchisq(2 * (log(dbinom(x = y, size = n, prob = phat) / dbinom(x = y, size = n, prob = pnull))), 1, lower.tail = FALSE)
    )
    return(LL)
}

# Replace 'n' and 'y' with specific values
n <- 100  # Sample size
y <- 60   # Observed successes

result <- likelihood_test(phat = 0.6, pnull = 0.5, y, n)
print(result)


#Binomial test
binom.test(y,n,conf.level = 0.95,alternative = 'two.sided')
install.packages('exactci')
library(exactci)

binom.exact(60,100,0.50,alternative = 'two.sided',mid=TRUE)

health<-c(25,115,145,90,29,484,1557,1309,545,11)
table_health<-as.table(matrix(health,nrow = 5,byrow = FALSE,dimnames = list(health=c('Excellent','Very good','Good','Fair','Poor'),Current.smoker=c('Yes','No'))))
table_health

addmargins(table_health)


result_per <- fisher.test(table_health, simulate.p.value = TRUE, B = 10000)
result_per

result_chisq <- chisq.test(table_health, simulate.p.value = TRUE, B = 10000)
result_chisq

install.packages('vcdExtra')
library(vcdExtra)

A=CMHtest(table_health, rscores = c(1,2,3,4,5), cscores = 0:1, types = "cor")
A

#sample correlation in R
sum_t=sum(table_health)
sum_t

sqrt_t=sqrt(A$table[1])
sqrt_t

R=sqrt_t/sqrt(sum_t-1)
R

p_value=pnorm(sqrt_t,lower.tail = FALSE)
p_value

# Check the p-value from the CMH test
p_value_1 <- A$p_value
p_value_1
# Set the significance level
alpha <- 0.05

# Compare the p-value with the significance level
if (p_value <= alpha) {
  cat("There is sufficient evidence to conclude that self-evaluation of health is associated with smoking status (α =", alpha, ")\n")
} else {
  cat("There is not sufficient evidence to conclude that self-evaluation of health is associated with smoking status (α =", alpha, ")\n")
}

#odds ratio
# Create a contingency table for the two groups: Excellent and Poor health
# Modify the table_health accordingly based on your data
table_health <- as.table(matrix(c(25, 29, 484, 11), nrow = 2, byrow = TRUE, dimnames = list(Health = c("Excellent", "Poor"), Current.smoker = c("Yes", "No"))))

# Calculate the odds ratio
odds_ratio <- table_health[1, 1] / table_health[1, 2] / (table_health[2, 1] / table_health[2, 2])
odds_ratio
# Print the odds ratio
cat("Odds Ratio:", odds_ratio, "\n")

odd_r=odds_ratio(c(29,11,25,484), method="wald", conf=0.95, correct=FALSE)


# Create a 2x2 table from the data
# Create a data frame with the counts
data <- data.frame(
  Health = c("Excellent", "Poor", "Excellent", "Poor"),
  Current.smoker = c("Yes", "Yes", "No", "No"),
  Count = c(25, 29, 484, 11)
)

# Create a 2x2 table from the data
table_data <- xtabs(Count ~ Health + Current.smoker, data = data)
table_data
# Calculate the odds ratio
odds_ratio <- table_data[1, 1] / table_data[1, 2] / (table_data[2, 1] / table_data[2, 2])

# Print the odds ratio
cat("Odds Ratio:", odds_ratio, "\n")


