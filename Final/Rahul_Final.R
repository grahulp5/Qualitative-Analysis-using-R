Alligators2 <- read.csv("~/Desktop/DANA-Qualt/Final/Alligators2.csv")
attach(Alligators2)

ztest=prop.test(3140,13819,alternative = "two.sided", correct = FALSE,conf.level = 0.95)
ztest
sqrt(ztest$statistic)

y<-3140
y
n<-13819
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
likelihood_test <- function(phat, pnull, y, n) {
  LL <- list(
    'LRTstatistic' = (2 * (log(dbinom(x = y, size = n, prob = phat) / dbinom(x = y, size = n, prob = pnull)))),
    'p-value' = pchisq(2 * (log(dbinom(x = y, size = n, prob = phat) / dbinom(x = y, size = n, prob = pnull))), 1, lower.tail = FALSE)
  )
  return(LL)
}

drinking_c<-prop.test(c(1392,1748),c(3956,6723),conf.level = 0.95, correct = FALSE)
oddsratio(drinking, method="wald", conf=0.95, correct=FALSE)


result <- likelihood_test(phat = 0.2272234, pnull = 0.5, y, n)
print(result)