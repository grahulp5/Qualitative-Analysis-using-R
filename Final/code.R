#Chapter 1

#Clinical exam
Clinical <- read.csv("~/Desktop/DANA-Qualt/Chapter 1/Clinical.csv", sep="")
View(Clinical)
attach(Clinical)

#Wald test Page 12
n=length(response)
n
y=sum(response)
y

phat=y/n
phat
SE=sqrt((phat*(1-phat))/n)
SE
z=(phat-0.5)/SE
z
pvalue1=2*pnorm(z,lower.tail = FALSE)
pvalue1
#or chi-square test statistic
z^2
pvalue2=pchisq(z^2,1,lower.tail = FALSE)
pvalue2



#Score test

ztest=prop.test(y,n, p = 0.5, alternative = "two.sided",
                correct = FALSE,conf.level=0.95)  
ztest
sqrt(ztest$statistic)

#likelihood-ratio test
likelihood_ratio_test <- function(phat, pnull, y,n)
{
  LL <- c("LRTstatistic"=2*log(dbinom(x = y, prob = phat, size = n)/dbinom(x = y, prob = pnull, size = n)),
          "p-value"=pchisq(2*log(dbinom(x = y, prob = phat, size = n)/dbinom(x = y, prob = pnull, size = n)),1,lower.tail = FALSE))
  return(LL)
}
likelihood_ratio_test(phat=.9,pnull=0.5,y,n)


#Binomial Exact test
binom.test(y,n,p=0.5, conf.level = 0.95, alternative = "two.sided")

#Binomial tests and confidence intervals using mid P-value
install.packages("exactci")
install.packages("PropCIs")
library(exactci)
binom.exact(9, 10, 0.50, alternative="two.sided", midp=TRUE) # mid P-value
library(PropCIs)
midPci(9, 10, 0.95)

#Clinical self
Clinical <- read.table("/Users/rahulgupta/Desktop/DANA-Qualt/Chapter 1/Clinical.dat", header=TRUE)
View(Clinical)
attach(Clinical)

#Wald test Page 12
n=length(response)
n
y=sum(response)
y

phat=y/n
phat
SE=sqrt((phat*(1-phat))/n)
SE
z=(phat-0.5)/SE
z
pvalue1=2*pnorm(z,lower.tail = FALSE)
pvalue1
#or chi-square test statistic
z^2
pvalue2=pchisq(z^2,1,lower.tail = FALSE)
pvalue2



#Score test

ztest=prop.test(y,n, p = 0.5, alternative = "two.sided",
                correct = FALSE,conf.level=0.95)  
ztest
sqrt(ztest$statistic)

#likelihood-ratio test
likelihood_ratio_test <- function(phat, pnull, y,n)
{
  LL <- c("LRTstatistic"=2*log(dbinom(x = y, prob = phat, size = n)/dbinom(x = y, prob = pnull, size = n)),
          "p-value"=pchisq(2*log(dbinom(x = y, prob = phat, size = n)/dbinom(x = y, prob = pnull, size = n)),1,lower.tail = FALSE))
  return(LL)
}
likelihood_ratio_test(phat=.9,pnull=0.5,y,n)


#Binomial Exact test
binom.test(y,n,p=0.5, conf.level = 0.95, alternative = "two.sided")

#Binomial tests and confidence intervals using mid P-value
install.packages("exactci")
install.packages("PropCIs")
library(exactci)
binom.exact(9, 10, 0.50, alternative="two.sided", midp=TRUE) # mid P-value
library(PropCIs)
midPci(9, 10, 0.95)

#defective
y=10
n=300
phat=y/n
z=qnorm(0.025,lower.tail = FALSE)
sd=sqrt((phat*(1-phat))/n)
c(phat-z*sd,phat+z*sd)

###
library(binom)
binom.confint(10, 300, conf.level = 0.95,
              method = "asymptotic")
###
ztest=prop.test(10, 300, p = 0.05, alternative = "less",
                correct = FALSE) 
sqrt(ztest$statistic)

#likelihood function
pi <- seq(0, 1, length=100)

h1 <- dbinom(0, 10, pi)
plot(pi, h1, type="l", lty=1, xlab="Binomial parameter π",
     ylab="Likelihood", main="Binomial Likelihood Functions", col="green")
h2= dbinom(6, 10, pi)
lines(pi, h2, col="red", lty=1)

legend("topright",c("Y=0","Y=6"), lty=c(1, 1), col=c("green", "red"))

##One sample Proportion test (large n)

ztest=prop.test(837, 1810, p = 0.5, alternative = "two.sided",
                correct = FALSE) # use"TRUE" if the expected number of successes or failures is less than 5) 
ztest
sqrt(ztest$statistic)

####Wald CI
install.packages("binom")
library(binom)
binom.confint(837, 1810, conf.level = 0.95, method = "asymptotic")
# Score CI, also called ``Wilson''
binom.confint(837, 1810, conf.level = 0.95, method = "wilson")
# Agresti-Coull CI
binom.confint(837, 1810, conf.level = 0.95, method = "ac")
#or
binom.confint(837, 1810, conf.level=0.95, method="agresti-coull")
#likelihood-ratio-test-based confidence interval for π
binom.confint(837, 1810, conf.level=0.95, method="lrt")

#Chapter 2

#fatality
Counts <- c(433,570,8049,554883)
Fatality.table <- as.table(matrix(Counts, nrow = 2, byrow = FALSE, 
                                  dimnames = list(
                                    Restraint.Use= c('No', 'Yes'), 
                                    Injury = c('Fatal', 'NonFatal'))))

Fatality.table


#Adding Totals 
addmargins(Fatality.table)

#create a data frame
Restraint.Use<- c("No", "Yes")
Injury<- c("Fatal", "NonFatal")
data <- expand.grid(Restraint.Use=Restraint.Use,Injury=Injury)
data
counts1 <- c(433,570,8049,554883)
data <- cbind(data, counts1)
data
##Odds Ratio
table <- xtabs(counts1 ~ Restraint.Use+Injury, data=data)
table
library(epitools)
oddsratio(Fatality.table, method="wald", conf=0.95, correct=FALSE)

###Wald CI for π1 − π2
prop.test(table,conf.level = 0.95, correct = FALSE)
#score confidence intervals π1 − π2
library(PropCIs)
diffscoreci(433, 8482, 570, 555453, conf.level=0.95)
#score CI for relative risk
library(PropCIs)
riskscoreci(433, 8482, 570, 555453, conf.level=0.95)

#chisqared distribution
x <- seq(0,40, length=1000)

h1 <- dchisq(x, 1)
plot(x, h1, type="l", lty=1, xlab="Chi-Squared",
     ylab="Probability Density", main="Chi-Squared Distributions", col="green", ylim=c(0,0.18))
h2= dchisq(x,5)
lines(x, h2, col="red", lty=1)
h3= dchisq(x,10)
lines(x, h3, col="black", lty=1)
h4= dchisq(x,20)
lines(x, h4, col="blue", lty=1)
legend("topright",c("df=1","df=5", "df=10","df=20"), lty=c(1, 1,1,1), col=c("green", "red", "black", "blue"))

#political
Political <- read.csv("~/Desktop/DANA-Qualt/Chapter 2/Political.dat", sep="")
View(Political)
attach(Political)
gen_par=Political[-c(1)]
gen_par
#Forms contingency table
Counts=table(gen_par)
Counts

#or 
GenderGap <- xtabs(~gender + party, data=Political)
GenderGap
#Adding Totals 
addmargins(Counts)
#Row Proportions with (Row=1, Column=2)
prop.table(Counts,margin = 1)
#Table with column percents
Column_percent =(prop.table(Counts,margin = 2)*100)
Column_percent
#Side-By-Side Bar graph by Flexibility
barplot(height =Column_percent , beside=TRUE, 
        col=c("red","blue"),legend.text = TRUE,
        args.legend = list(x="topright"), ylim=c(0,150),
        xlab="Political Party Identification", 
        ylab="Percent", 
        main = "Side-By-Side Bar graph for Gender by Political Party")

#Mosaic plot

install.packages("vcd")
install.packages("grid")
library(grid)
library(vcd)
mosaic(GenderGap, gp=shading_Friendly, residuals=stdres, 
       residuals_type="Std\nresiduals", labeling=labeling_residuals)

#Chi-Square test
test=chisq.test(Counts)
test

#Observed and Expected Counts
Obesrved=test$observed
Obesrved
Expected=test$expected
Expected
#Individual cell contribution
cont=(Obesrved-Expected)^2/Expected
cont
# standardized residuals
stdres <- test$stdres
stdres
#Likelihood-Ratio Statistic
library(MASS)
loglm(~gender + party, data=GenderGap)

#death penalty#create a data frame
Victims.Race<- c("White", "Black")
Death.Penalty<- c("Yes", "No")
Defendant.Race <- c("White", "Black")
data <- expand.grid(Defendant.Race=Defendant.Race,Victims.Race=Victims.Race, Death.Penalty=Death.Penalty)
counts <- c(53,11,0,4,414,37,16,139)
data <- cbind(data, counts)
data

# make it into a 3-way table
t <- xtabs(counts ~ Victims.Race + Defendant.Race+Death.Penalty, data=data)
t
Verdict=ftable(t,row.vars = c(1,2))
Verdict
#adding margins

ftable(addmargins(t,margin = 2:3,list(Total=sum)))

#Percentage of Yes
Percentage=prop.table(Verdict,margin = 1)*100
Percentage
#Percentage of yes ignoring victims’ race
A=ftable(addmargins(t,margin = 1:2,list(Total=sum)))
Percentage1=prop.table(A,margin = 1)*100
Percentage1
#Side-By-Side Bar graph
install.packages("latticeExtra")
library(latticeExtra)
data1=data[data$Death.Penalty=='Yes',]
data1

cloud(Percentage[,1]~data1$Victims.Race+data1$Defendant.Race, data, panel.3d.cloud=panel.3dbars, col.facet=c('red'), 
      xbase=1, ybase=1, scales=list(arrows=FALSE, col=2,distance =c(3, 3, 4)), 
      par.settings = list(axis.line = list(col = "green")), 
      zlab = "Percent Yes",ylab = "Defendant Race", xlab="Victims Race",
      screen = list(z = 15, x = -50, y=0), zoom=0.5)

#Ignoring victims’ race
dataIg <- expand.grid(Defendant.Race=Defendant.Race, Death.Penalty=Death.Penalty)
counts <- c(53,15,430,176)
data2 <- cbind(dataIg, counts)
data2

# make it into a 2-way table
t2 <- xtabs(counts ~ Defendant.Race+Death.Penalty, data=data2)

VerdictYes=ftable(t2,row.vars = 1)
VerdictYes
#adding margins

ftable(addmargins(VerdictYes))

#Percentage of Yes
PercentageYes=prop.table(VerdictYes,margin = 1)*100
PercentageYes
barplot(PercentageYes[,1]~Defendant.Race,main = "Imposed Death Penalty Ignoring victims’ race ",
        xlab = "Defendant’s Race",
        ylab = "Percentage of Yes (%)",
        col = "lightblue")
##Odds Ratio between Victims' Race and Defendants' Race 
ttt <- xtabs(counts ~ Victims.Race + Defendant.Race, data=data)
ttt
Race=ftable(ttt,row.vars =2 )
Race
library(epitools)
oddsratio(Race, method="wald", conf=0.95, correct=FALSE)
##Odds Ratio between Death Penalty and Defendants' Race when victims’ race is white
tab1 <- xtabs(counts ~ Defendant.Race+Death.Penalty , Victims.Race=="White", data=data)
tab1
oddsratio(tab1, method="wald", conf=0.95, correct=FALSE)
##Odds Ratio between Death Penalty and Defendants' Race when victims’ race is black
tab2 <- xtabs(counts ~ Defendant.Race+Death.Penalty, Victims.Race=="Black", data=data)
tab2
oddsratio(tab2, method="wald", conf=0.95, correct=FALSE)
##Odds Ratio between Death Penalty and Defendants' Race
tab3 <- xtabs(counts ~ Defendant.Race+Death.Penalty, data=data)
tab3
oddsratio(tab3, method="wald", conf=0.95, correct=FALSE)

#fischer tea
tea <- matrix(c(3,1,1,3), ncol=2)
# two-sided Fisher's Exact Test
fisher.test(tea)
#one-sided alternative hypothesis: true odds ratio is not equal to 1
fisher.test(tea, alternative = "greater")
#Mid P-values for testing independence
library(epitools)
ormidp.test(3, 1, 1, 3, or=1)
#mid-P confidence interval for odds ratio
library(epitools)
or.midp(c(3, 1, 1, 3), conf.level=0.95)$conf.int

###Hypergeometric distribution, x, m=#S,n=#F, k=n 
p_3= dhyper(3,4,4,4)
p_3
p_4= dhyper(4,4,4,4)
p_4
p_value=1-phyper(2,4,4,4)
p_value

####
n11=c(0,1,2,3,4)


##Example
dhyper(0,4,6,3)

#hyper function

n11=c(0,1,2,3,4)
p_3= dhyper(3,4,4,4)
p_value=1-phyper(2,4,4,4)
#likelihood-ratio test
p_p_value <- function(x)
{
  p_x <- c("Probability"=dhyper(x,4,4,4),
           "p-value"=1-phyper(x-1,4,4,4))
  return(p_x)
}
p_p_value(n11)
cbind(n11,"Probability" = p_p_value(n11)[1:5], "P -value"=p_p_value(n11)[6:10] )

#malform
Malform <- c(17066, 14464, 788, 126, 37, 48, 38, 5, 1, 1)
Malform.table <- as.table(matrix(Malform, nrow = 5, byrow = FALSE, 
                                 dimnames = list(Alcohol.Consumption= c('0', '< 1', '1-2', '3-5', '>= 6'), 
                                                 Malformation = c('Absent', 'Present'))))
Malform.table
#Adding Totals 
addmargins(Malform.table)
# Testing the linear trend
install.packages("vcdExtra")
library(vcdExtra)
A=CMHtest(Malform.table, rscores = c(0, 0.5, 1.5, 4.0, 7.0), cscores=0:1, types="cor")
A
#Sample correlation R
n=sum(Malform.table)
n
M=sqrt(A$table[1])
M
R=M/sqrt(n)
R
#P-value for one-sided standard normal with M statistic

P_value=pnorm(M, lower.tail = FALSE)
P_value



#physicians health
#Wald CI for π1 − π2

prop.test(c(189, 104), c(11034, 11037),conf.level = 0.95, correct = FALSE)

#score confidence intervals π1 − π2
library(PropCIs)
diffscoreci(189, 11034, 104, 11037, conf.level=0.95)



#score CI for relative risk
library(PropCIs)
riskscoreci(189, 11034, 104, 11037, conf.level=0.95)

## Wald CI for odds ratio, no continuity correction uses the four cell counts
install.packages("epitools")
library(epitools)
oddsratio(c(189,10845,104,10933), method="wald", conf=0.95, correct=FALSE)
# score CI for odds ratio uses success count and n for each binomial group
library(PropCIs)
orscoreci(189, 11034, 104, 11037, conf.level=0.95)

#Chapter 3

#crabs
Crabs <- read.csv("~/Desktop/Langara/DANA 4820/Data Sets - updated Apr 2019/Crabs.dat", sep="")
View(Crabs)
attach(Crabs)
head(Crabs)
##Scatter plot
plot(x=width, y=sat, ylab = "Number of satellites", xlab = "width (cm)", 
     col="blue", ylim = c(0,17), xlim = c(20,34), pch=19)
#Poisson log linear model with Log Link

fit <- glm(sat ~ width, family=poisson(link=log), data=Crabs)
summary(fit)

#Predict and transform

pred.poi_log= exp(predict(fit)) 

#fitted value at the mean width of x = 26.3

newdata=data.frame(width=mean(width))
muhat=predict(fit, newdata, , type = "response")
muhat

#exp(βˆ)
exp(fit$coefficients[2])



#Generalized additive model
library(gam) 
# generalized additive model smoothing fit which 
#s() is smooth function predictor for generalized additive model
gam.fit <- gam(sat ~ s(width), family=poisson, data=Crabs)
# Adding the smoothed curve
curve(predict(gam.fit, data.frame(width=x), type="resp"), add=TRUE, col="red")

#log link vs. identity link
plot(x=width, y=sat, ylab = "Number of satellites", xlab = "width (cm)", 
     col="blue", ylim = c(1,17), xlim = c(20,34), pch=19)

#Poisson regression model with identity link function 

fit1 <- glm(sat ~ width, family=poisson(link="identity"), data=Crabs, start=c(0.5,0.5))

summary(fit1)

#Creating x values
q=seq(min(width), max(width), 0.01)
newdata1=data.frame(width=q)
pred.poi_log= predict(fit, newdata1,type = "response" )


pred.poi_id111 = predict(fit1, newdata1, type = "response")

lines(q, pred.poi_log, col="red")
lines(q, pred.poi_id, col="green")

legend("topleft", bty="n",
       legend=c("Log Link", 
                "Identity Link"),
       lty=c( "solid", "solid"),
       col=c("red", "green"),
       lwd=c(1,1))

#evolution
Evolution <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch3/Data/Evolution.dat", sep="")
attach(Evolution)
head(Evolution)

# binomial sample sizes

n <- Evolution$true + Evolution$false

#fitting GLM

fit <- glm(true/n ~ ideology, family=binomial, weights=n, data=Evolution)
summary(fit)

#Wald test




#CI
confint(fit)

# likelihood-ratio tests for effect parameters in a GLM
library(car)
Anova(fit)
#or
drop1(fit, test="LRT")
#Score test
install.packages("statmod")
library(statmod)
fit0 <- glm(true/n ~ 1, family=binomial, weights=n, data=Evolution)
summary(fit0)
# squaring a z score statistic
a=(glm.scoretest(fit0, Evolution$ideology))^2
a
#p-value
pchisq(a,1,lower.tail = FALSE)

##p-value for Deviance 
D=deviance(fit)
D
df.residual(fit)
p_value = pchisq(D,df.residual(fit),lower.tail = FALSE)
p_value

##Model Comparison Using the Deviance
G = fit$null.deviance - fit$deviance
G
dfG = fit$df.null - fit$df.residual
dfG

#Standardized Residuals

cbind(ideology,true,false,n,phat =true/n,fitted=fitted(fit),std_res=rstandard(fit,type="pearson"))


#logistic regression curve


x <- seq(0,20, length=100)


pi=exp(-5+0.7*x)/(1+exp(-5+0.7*x))
plot(x, pi, type="l", lty=1, xlab="x",
     ylab="π(x)", main="Logistic regression functions", col="green", ylim = c(0,1))
pi2= exp(5-0.7*x)/(1+exp(5-0.7*x))
lines(x, pi2, col="red", lty=1)

legend("topright",c("β>0","β<0"), lty=c(1, 1), col=c("green", "red"))

#multiple heart
Heart2 <- read.csv("~/Desktop/DANA-Qualt/Chapter 3/Heart2.dat", sep="")
View(Heart2)
attach(Heart2)
head(Heart2)
#Frequencies of the levels of snoring  
table(Heart2$snoring)
#Assigning codes for the levels of snoring
library(dplyr)

Heart2$snoring<- recode(Heart2$snoring,never = 0, occas= 2, nearly= 4, every= 5)
tail(Heart2)
#fitting logistic regression model
L=glm(y~snoring,data=Heart2, family = binomial(link = "logit"))
summary(L)
#fitted values (probability estimates) at 4 levels of snoring
N=data.frame(snoring=c(0,2,4,5))
LogisticFit=predict(L,newdata=N,type = "response")
LogisticFit
#linear probability model
fit2 <- glm(y~snoring, family=quasi(link=identity, variance="mu(1-mu)"),data=Heart2)
summary(fit2, dispersion=1)
LinearFit=predict(fit2,newdata=N,type = "response")
LinearFit

#Table 3.1
Table=ftable(Heart2$snoring,Heart2$y)
ProportionYes=prop.table(Table,margin = 1)[,2]
ProportionYes
cbind(Yes=Table[,2],ProportionYes,LinearFit,LogisticFit)
##plotting the sample proportions and the fitted values


plot( x= levels(factor(Heart2$snoring)),y=ProportionYes , main="Fitted models for snoring and heart disease data",
      pch=19, col="blue",
      xlab = "Snoring", 
      ylab = " Predicted Probability",  ylim = c(0, 0.2),  
      xlim = c(0, 5))
lines(LinearFit~levels(factor(Heart2$snoring)),lwd=2,col="green")
lines(predict(L,type = "response")~Heart2$snoring,lwd=2,col="red")
#or
lines(LogisticFit~levels(factor(Heart2$snoring)),lwd=2,col="red")
legend("topleft", legend = c("Linear", "Logistic"),pch=19, col = c("Green", "Red"))



#heart
Heart <- read.csv("~/Desktop/DANA-Qualt/Chapter 3/Heart.dat", sep="")
View(Heart)
attach(Heart)
head(Heart)
install.packages('dplyr')
library(dplyr)
Heart$x<- recode(Heart$snoring,never = 0, occasional= 2, nearly_every_night= 4, every_night= 5)
tail(Heart)
n <- Heart$yes + Heart$no # binomial sample sizes are the row totals
n
fit <- glm(yes/n ~ x, family=binomial(link=logit), weights=n, data=Heart)
# canonical link for binomial is logit, so "(link=logit)" not necessary
summary(fit)
a=fit$coefficients[1]
b=fit$coefficients[2]
phat=exp(a+b*0)/(1+exp(a+b*0))
phat
#meaning that the probability of heart disease when snoring 0
#is 2%
fit2 <- glm(yes/n ~ x, family=quasi(link=identity, variance="mu(1-mu)"), weights=n, data=Heart)
summary(fit2,dispersion=1)


#chapter 4

#smoklung
Cancer<- c("Cases", "Controls")
Smoker<- c("Yes", "No")

data <- expand.grid( Smoker=Smoker, Cancer=Cancer)
data
counts <- c(688,21,650,59)
data <- cbind(data, counts)
data
# make it into a 2-way table
t2 <- xtabs(counts ~ Smoker+Cancer, data=data)
t2

####logistic Regression
Cases=c(688,21)
Controls=c(650,59)
n=Cases+Controls
Smoker=c(1,0)
Smlung=data.frame(cbind(Smoker,Cases,Controls ))
Smlung
library(epitools)
oddsratio(c(688,650,21,59), method="wald", conf=0.95, correct=FALSE)

fit<- glm(Cases/n~Smoker, family=binomial, weights=n, data=Smlung)
summary(fit)
exp(fit$coefficients[2])

#crab
Crabs <- read.csv("~/Desktop/DANA-Qualt/Chapter 3/Crabs.dat", sep="")
attach(Crabs)
head(Crabs)
##scatter plot of y by x=width
plot(x=width, y=y, ylab = "Satellites", xlab = "Width (cm)", 
     col="blue", ylim = c(0,1), xlim = c(20,34), pch=19)

##scatter plot of y by x=width that jitters the data points that is add random noise.
plot(jitter(y, 0.5) ~  width, data=Crabs,ylab = "Satellites", xlab = "Width (cm)", 
     col="blue", ylim = c(-0.1,1.5), xlim = c(20,34))

#logistic regression fit
fit<-glm(y~width,family = binomial,data=Crabs)
summary(fit)

#predicting model for fit
predict(fit,data.frame(width=c(mean(width),max(width))),type='response')

predict(fit,data.frame(width=c(mean(width))),type='response')

min(width)
max(width)

#calculate the median effective level at 50%
##x=(-a/b) #where a is alpha and b is beta
#here a=-12.3508 and b is 0.4972
x=-(-12.3508/0.4972)
x

mean(width)

exp(0.4972)


#Fitting generalized additive models
install.packages("gam")
library(splines)
library(foreach)
library(gam)

#s below for s(width) means smooth function of predictor variable width
#deviance residual tells how well the model fits the data


gam.fit <- gam(y ~ s(width), family=binomial, data=Crabs) # s = smooth funct.

summary(gam.fit)

# plots generalized additive model smoothing fit
curve(predict(gam.fit, data.frame(width=x), type="resp"), add=TRUE,col="black")

fit <- glm(y ~ width, family=binomial, data=Crabs) # link=logit is default
# logistic regression fit is added to the plot
curve(predict(fit, data.frame(width=x), type="resp"), add=TRUE, col="red")
summary(fit)

legend("topleft",c("Generalized Additive Fit","Logistic Regression Fit"), lty=c(1, 1), col=c("black", "red"))

# estimated probability of satellite at min(width) and max(width)
predict(fit, data.frame(width=c(min(width), max(width))), type="response")
# estimated probability of satellite at mean width
predict(fit, data.frame(width = mean(Crabs$width)), type="response")

#The median effective level

fit$coefficients[1]/fit$coefficients[2]

##
exp(fit$coefficients[2])

##Profile CI 
P=confint(fit)
P
P[2,]
# likelihood-ratio test of width effect
library(car)
Anova(fit)
##Wald CI
W=confint.default(fit)
W

##Exponentiating the endpoints 
##Profile CI
exp(P[2,])
#Wald CI
exp(W[2,])

# estimated probability of satellite at x = 26.5
New=data.frame(width=c(26.5))
Fitted=predict(fit, New, type="response")
Fitted
# ML fitted value estimate of P(Y=1)
pred.prob <- fitted(fit) 
# linear predictor
lp=predict(fit,New, se.fit=TRUE)
lp
alfa=0.05
a=1-(alfa/2)
z = qnorm(a)
z
lp$fit + c(-1, 1) * z * lp$se.fit
# confidence bounds for P(Y=1)
CI=exp(lp$fit + c(-1, 1) * z * lp$se.fit)/(1+exp(lp$fit + c(-1, 1) * z * lp$se.fit))
CI
##or
install.packages("boot")
library(boot)
CI=inv.logit(lp$fit + c(-1, 1) * z * lp$se.fit)
CI
# fitted value and confidence bounds for P(Y=1)
cbind(Fitted,"CI:LB"=CI[1], "CI:UB"=CI[2])

###Graph
plot(jitter(y, 0.5) ~  width, data=Crabs,ylab="Prob(satellite)", xlab = "Width (cm)", 
     col="blue", ylim = c(-0.1,1.5), xlim = c(18,34), pch=16)
data.plot <- data.frame(width=(18:34))
lp <- predict(fit, newdata=data.plot, se.fit=TRUE)
pred.prob <- exp(lp$fit)/(1 + exp(lp$fit))
LB <- lp$fit - qnorm(0.975)*lp$se.fit
UB <- lp$fit + qnorm(0.975)*lp$se.fit
LB.p <- exp(LB)/(1 + exp(LB)); UB.p <- exp(UB)/(1 + exp(UB))
lines(18:34, pred.prob)
lines(18:34, LB.p, col="red"); lines(18:34, UB.p, col="blue")
legend("topleft",c("Lower bound","Upper Bound", "Fitted"), lty=c(1, 1), col=c("red", "blue", "black"))
###Sample Proportion
n=length(which(Crabs$width ==26.5))
x=length(which(Crabs$width ==26.5&Crabs$y==1))
phat=x/n
phat
SE=sqrt(phat*(1-phat)/n)
SE
#Model-based estimate
predict(fit, New, type="response",se.fit=TRUE)


#crab multiple
Crabs <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch4/Data/Crabs.dat", sep="")
View(Crabs)
attach(Crabs)
head(Crabs)

##Model fitting:
c2=ifelse(color==2,1,0)
c3=ifelse(color==3,1,0)
c4=ifelse(color==4,1,0)
fit1 <- glm(y ~ width+c2+c3+c4, family=binomial, data=Crabs) # link=logit is default
summary(fit1)

##or
fit2 <- glm(y ~ width+factor(color), family=binomial, data=Crabs) # link=logit is default
summary(fit2)
#parameter estimate for the variable width
exp(fit1$coefficients[2])
###Graph
plot(jitter(y, 0.5) ~  width, data=Crabs,ylab="Prob(satellite)", xlab = "Width (cm)", 
     col="blue", ylim = c(-0.1,1.5), xlim = c(18,34), pch=16)

curve(predict(fit1, data.frame(width=x ,c2=c(0),c3=c(0),c4=c(0)), 
              type="resp"), add=TRUE, col="red")
curve(predict(fit1, data.frame(width=x ,c2=c(1),c3=c(0),c4=c(0)), 
              type="resp"), add=TRUE, col="blue")
curve(predict(fit1, data.frame(width=x ,c2=c(0),c3=c(1),c4=c(0)), 
              type="resp"), add=TRUE, col="green")
curve(predict(fit1, data.frame(width=x ,c2=c(0),c3=c(0),c4=c(1)), 
              type="resp"), add=TRUE, col="black")
legend("topleft",c("Color 1","Color 2","Color 3","Color 4"), 
       lty=c(1, 1), col=c("red", "blue","green","black"), cex=0.7)
#Comparing Estimated probabilities when width is the average value
mean(width)
Predicted_P_Color_1=predict(fit1, data.frame(width=c(mean(width)) ,
                                             c2=c(0),c3=c(0),c4=c(0)), type="response")
Predicted_P_Color_1
Predicted_P_Color_2=predict(fit1, data.frame(width=c(mean(width)) ,
                                             c2=c(1),c3=c(0),c4=c(0)), type="response")
Predicted_P_Color_2
Predicted_P_Color_3=predict(fit1, data.frame(width=c(mean(width)) ,
                                             c2=c(0),c3=c(1),c4=c(0)), type="response")
Predicted_P_Color_3
Predicted_P_Color_4=predict(fit1, data.frame(width=c(mean(width)) ,
                                             c2=c(0),c3=c(0),c4=c(1)), type="response")
Predicted_P_Color_4
X=cbind(round(Predicted_P_Color_1,4),round(Predicted_P_Color_2,4),
        round(Predicted_P_Color_3,4),round(Predicted_P_Color_4,4))
colnames(X)=c("Color 1", "Color 2", "Color 3", "Color 4")
rownames(X)=c("Predicted Probability")
X

###Odds
odds1to4=exp(0-fit1$coefficients[5])
odds1to4
##odds 
odds4=Predicted_P_Color_4/(1-Predicted_P_Color_4)
odds4
odds1=Predicted_P_Color_1/(1-Predicted_P_Color_1)
odds1
odds1_4=odds1/odds4
odds1_4
###Model Reduced
fit3 <- glm(y ~ width, family=binomial, data=Crabs) # link=logit is default
summary(fit3)
###Model Full
fit1 <- glm(y ~ width+c2+c3+c4, family=binomial, data=Crabs) # link=logit is default
summary(fit1)
##Deviance
G = fit3$deviance -fit1$deviance
G
df=fit3$df.residual-fit1$df.residual
P_Value=pchisq(G,df,lower.tail = FALSE)
P_Value
##Anova
library(car)
Anova(fit2)

##Color as a numerical variable

fit6 <- glm(y ~ width+color, family=binomial, data=Crabs)
summary(fit6)
###comparing fit 6 and fit 2

anova(fit6,fit1, test="LRT")

##fit 5, for dark-colored crabs

fit5 <- glm(y ~ width+c4, family=binomial, data=Crabs) # link=logit is default
summary(fit5)
# likelihood-ratio test comparing models
anova(fit5,fit1,test="LRT")

##Interaction

fit4 <- glm(y ~ width+c4+width:c4, family=binomial, data=Crabs)
summary(fit4)
###Model Reduced without interaction
fit5 <- glm(y ~ width+c4, family=binomial, data=Crabs) # link=logit is default
summary(fit5)
###Model Full with interaction
fit4 <- glm(y ~ width+c4+width:c4, family=binomial, data=Crabs)
summary(fit4)
##Deviance
G = fit5$deviance -fit4$deviance
G
df=fit5$df.residual-fit4$df.residual
P_Value=pchisq(G,df,lower.tail = FALSE)
P_Value

##Predictive Power using phat
n=length(width)
yes=sum(y)
phat=yes/n
phat # sample proportion of 1's for y variable
predicted1 <- as.numeric(fitted(fit1) > phat) # predict y=1 when est.> 0.6416
xtabs(~y + predicted1) # Classification table with sample proportion cutoff
##Predictive Power using p0 = 0.5
predicted2 <- as.numeric(fitted(fit1) > 0.5) # predict y=1 when est.> 0.5
A=xtabs(~y + predicted2) # Classification table with sample proportion cutoff
addmargins(A,2)
##ROC Curve
install.packages("pROC")
library(pROC)
rocplot <- roc(y ~ fitted(fit2), data=Crabs)
plot.roc(rocplot, legacy.axes=TRUE) # Specficity on x axis if legacy.axes=F
auc(rocplot) # auc = area under ROC curve = concordance index
##R
R1=cor(y, fitted(fit1))
R2=cor(y, fitted(fit3))
R3=cor(y, fitted(fit4))
R4=cor(y, fitted(fit5))
X=cbind(round(R1,4),round(R2,4),
        round(R3,4),round(R4,4))
colnames(X)=c("width&Color", "width", "interaction", "width&color 4")
rownames(X)=c("R")
X
###
rocplot1 <- roc(y ~ fitted(fit1), data=Crabs)
rocplot3 <- roc(y ~ fitted(fit3), data=Crabs)
rocplot4 <- roc(y ~ fitted(fit4), data=Crabs)
rocplot5 <- roc(y ~ fitted(fit5), data=Crabs)
rocplot6 <- roc(y ~ fitted(fit6), data=Crabs)
plot.roc(rocplot3, legacy.axes=TRUE) # Specficity on x axis if legacy.axes=F
A1=auc(rocplot1) # auc = area under ROC curve = concordance index
A2=auc(rocplot1) 
A3=auc(rocplot1) 
A4=auc(rocplot1) 
A5=auc(rocplot1) 
Z=cbind(round(A1,4),round(A2,4),
        round(A3,4),round(A4,4),round(A5,4))
colnames(Z)=c("Full:width&Color", "width", "interaction", "width&color 4","Numerical Color")
rownames(Z)=c("auc")
Z

#marijuana
Marijuana <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch4/Data/Marijuana.dat", sep="")
attach(Marijuana)
View(Marijuana)
n=yes+no
n
fit <- glm(yes/(yes+no) ~ gender + race, weights =n, family=binomial, data=Marijuana)
summary(fit)
#Estimated conditional odds ratio between marijuana use and gender 
exp(fit$coefficients[2])
##Tests to measure overall goodness-of-fit 

##Test 1
D=deviance(fit)
D
df=df.residual(fit)
df
p_value1=pchisq(D,df, lower.tail = FALSE)
p_value1

##Test 2
G=fit$null.deviance-fit$deviance
G
df_G=fit$df.null-fit$df.residual
df_G
p_value2=pchisq(G,df_G, lower.tail = FALSE)
p_value2
#Test 3: The likelihood-ratio test 
install.packages("lmtest")
library(lmtest)
lrtest(fit)
##The likelihood-ratio test for individual explanatory variables
library(car)
Anova(fit)

#chapter 5

#admission
n=yes+no
head(Admissions)

fit <- glm(yes/(yes+no) ~ gender, weights =yes+no, family=binomial, data=myData)
summary(fit)
rstandard(fit,type="pearson")
Goodness<- function(x) {
  G1=fit$deviance
  df=fit$df.residual
  pvalue=pchisq(G1,df,lower.tail = FALSE)
  cat("Deviance =", G1, "df =", df, " p-value =", pvalue)
}
Goodness(fit)
myData = Admissions[Admissions$gender > 0,]

# ch5
Crabs <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch4/Data/Crabs.dat", sep="")
View(Crabs)
attach(Crabs)
head(Crabs)
##Dummy variables for color and spine condition
table(spine)# to find out how many dummy variables we need
c2=ifelse(color==2,1,0)
c3=ifelse(color==3,1,0)
c4=ifelse(color==4,1,0)
s2=ifelse(spine==2,1,0)
s3=ifelse(spine==3,1,0)
##Full Model
length(Crabs[which(y=="1"),][,3])
length(Crabs[which(y=="0"),][,3])

ModelA=glm(y ~ weight+width+c2+c3+c4+s2+s3, family=binomial, data=Crabs) # link=logit is default
summary(ModelA)
#or
ModelAA=glm(y ~ weight+width+factor(color)+factor(spine), family=binomial, data=Crabs) # link=logit is default
summary(ModelAA)
##Test for main effects
G=Model11$null.deviance-Model11$deviance
G
df=Model11$df.null-Model11$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
#likelihood-ratio tests for individual explanatory variables
library(car)
Anova(Model11)

##Model Comparison
Model1=glm(y ~ 1, family=binomial, data=Crabs)#null model 
summary(Model1)
Model2=glm(y ~ factor(color), family=binomial, data=Crabs)#Color 
summary(Model2)
##Test for main effect color
G=Model1$deviance-Model2$deviance
G
df=Model1$df.residual-Model2$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
##
Model3=glm(y ~ factor(spine), family=binomial, data=Crabs)#Spine
summary(Model3)
##Test for main effect spine
G=Model1$deviance-Model3$deviance
G
df=Model1$df.residual-Model3$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
Model4=glm(y ~ width, family=binomial, data=Crabs)#width
summary(Model4)
##Test for main effect width
G=Model1$deviance-Model4$deviance
G
df=Model1$df.residual-Model4$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
Model5=glm(y ~ width+c2+c3+c4, family=binomial, data=Crabs) #Color and width
summary(Model5)
##Test for comparing Model 5 and Model 2
G=Model2$deviance-Model5$deviance
G
df=Model2$df.residual-Model5$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
##Test for comparing Model 5 and Model 4
G=Model4$deviance-Model5$deviance
G
df=Model4$df.residual-Model5$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
Model6=glm(y ~ width+c2+c3+c4+s2+s3, family=binomial, data=Crabs) #Color and width and spine
summary(Model6)
##Test for comparing Model 6 and Model 5
G=Model5$deviance-Model6$deviance
G
df=Model5$df.residual-Model6$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
Model7=glm(y ~ width+factor(color)+width*factor(color), family=binomial, data=Crabs) #Color and width and interaction
summary(Model7)
##Test for comparing Model 5 and Model 7
G=Model5$deviance-Model7$deviance
G
df=Model5$df.residual-Model7$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue
##AIC
-2*logLik(Model5)
AIC(Model5)

##Step wise
ModelAA=glm(y ~ weight+width+factor(color)+factor(spine), family=binomial, data=Crabs) # link=logit is default
summary(ModelAA)
library(MASS)
stepAIC(ModelAA) # stepwise backward selection using AIC

##Alternative way using bestglm
Crabs2=data.frame(weight, width, color, spine, y) # y in last column
install.packages("leaps")
install.packages("bestglm")
library(leaps)
library(bestglm)
bestglm(Crabs2, family=binomial, IC="AIC") # can also use IC="BIC"

Model8=glm(y ~ width+factor(color)+width*factor(color)+factor(spine)+width*factor(spine), family=binomial, data=Crabs) #Color and width and interaction
summary(Model8)
stepAIC(Model8)
library(pROC)
rocplot5 <- roc(y ~ fitted(Model5), data=Crabs)
rocplot4 <- roc(y ~ fitted(Model4), data=Crabs)
A5=auc(rocplot5) 
A4=auc(rocplot4)
plot.roc(rocplot4, legacy.axes=TRUE, col="red")
plot.roc(rocplot5, legacy.axes=TRUE, col="blue", add = TRUE)
legend("topleft",legend=c("Model4","Model5"),col=c("red","blue"),lty=1:1)

####Goodness of Fit for Marijuana Use Survey
Marijuana <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch4/Data/Marijuana.dat", sep="")
attach(Marijuana)
View(Marijuana)
n=yes+no
n
fit <- glm(yes/(n) ~ gender + race, weights =n, family=binomial, data=Marijuana)
summary(fit)

#Deviance Function

Goodness<- function(x) {
  G1=x$deviance
  df=x$df.residual
  pvalue=pchisq(G1,df,lower.tail = FALSE)
  cat("Deviance =", G1, "df =", df, " p-value =", pvalue)
}

Goodness(fit)

#estimated prob's of marijuana use
fitted(fit)

fit.yes <- n*fitted(fit); fit.no <- n*(1 - fitted(fit))
data.frame(race, gender, yes, fit.yes, no, fit.no)


predict(fit,data.frame(gender=c("female"), race=c("white")), type = "response")
#standardized residuals
rstandard(fit,type="pearson")
library(pROC)
plot(roc(yes,no))

#marijuana
Marijuana <- read.csv("~/Desktop/Langara/DANA 4820/Nooshin/Ch4/Data/Marijuana.dat", sep="")
attach(Marijuana)
View(Marijuana)
race=c("white")
gender=c("female")
y=c(1)
D1=data.frame(race,gender,y)
A1=D1[rep(seq_len(nrow(D1)),420),]
View(A1)
#
race=c("white")
gender=c("female")
y=c(0)
D2=data.frame(race,gender,y)
A2=D2[rep(seq_len(nrow(D2)),620),]
View(A2)
#
race=c("white")
gender=c("male")
y=c(1)
D3=data.frame(race,gender,y)
A3=D3[rep(seq_len(nrow(D3)),483),]
View(A3)
#
race=c("white")
gender=c("male")
y=c(0)
D4=data.frame(race,gender,y)
A4=D4[rep(seq_len(nrow(D4)),579),]
View(A4)
#
race=c("other")
gender=c("female")
y=c(1)
D5=data.frame(race,gender,y)
A5=D5[rep(seq_len(nrow(D5)),25),]
View(A5)
#
race=c("other")
gender=c("female")
y=c(0)
D6=data.frame(race,gender,y)
A6=D6[rep(seq_len(nrow(D6)),55),]
View(A6)
#
race=c("other")
gender=c("male")
y=c(1)
D7=data.frame(race,gender,y)
A7=D7[rep(seq_len(nrow(D7)),32),]
View(A7)
#
race=c("other")
gender=c("male")
y=c(0)
D8=data.frame(race,gender,y)
A8=D8[rep(seq_len(nrow(D8)),62),]
View(A8)

A=rbind(A1,A2,A3,A4,A5,A6,A7,A8)
View(A)
attach(A)
table(A$gender)
fit <- glm(A$y ~ factor(A$gender) + factor(A$race), family=binomial, data=A)
summary(fit)

##
library(pROC)
rocplot <- roc(A$y ~ fitted(fit), data=A)
plot.roc(rocplot, legacy.axes=TRUE, col="red")
table(fitted(fit))

#chapter 6

Alligators <- read.csv("~/Desktop/DANA-Qualt/Chapter 6/Alligators.dat", sep="")
View(Alligators)
attach(Alligators)
table(y)

c1<-ifelse(y==c('F'),1,ifelse(y==c('I'),2,3))
x<-cbind(c1)

tail(x)
install.packages("VGAM")
library(stats4)
library(splines)
library(VGAM)


fit<-vglm(y~x, family=multinomial, data=Alligators)
summary(fit,HDEtest=FALSE)

coef(fit,matrix=TRUE)

fit2<-vglm(factor(y)~x, family = multinomial(refLevel = "I"), data = Alligators)
summary(fit2,HDEtest=FALSE)

coef(fit2,matrix=TRUE)

#profile likelihood confidence intervals
confint(fit2,method="profile")

#likelihodd ratio test statistic
fit0<-vglm(y~1, family=multinomial, data=Alligators) #null model
deviance(fit0)

deviance(fit)

#LR tests
lrtest(fit,fit0)

#likelihood confidence intervals for beta1 and beta2
confint(fit2, method="profile")

#calculate the estimated probabilities for an alligator of maximum observed length
length=Alligators$x
fit1<-vglm(y~length, family=multinomial, data=Alligators)
max(length)

New=data.frame(length=c(max(length)))
predict(fit1, New, type="response")

#negative beta value means pi decreases as x increases

exp(-0.110109)
exp(-2.465446)





#activity
#cumulative model
install.packages("VGAM")
library(stats4)
library(splines)
install.packages("car")
library(VGAM)

y1=c(21,53,94)
y2=c(159,372,249)
y3=c(110,221,83)
Income=c('Aaverage','Averge','Baverage')
happiness=data.frame(Income,y1,y2,y3)
fit<-vglm(cbind(y1,y2,y3)~Income,family=cumulative(parallel = TRUE),data=happiness)
summary(fit)

exp(0.1516)
#The estimated odds that the person above averge income
#is less happy is 1.16 times the estimated odds for perople
#who are above average income.

exp(1.0709)

x1=ifelse(Income=="Baverage",'1',ifelse(Income=="Averge",'2','3'))
Happiness2 = data.frame(x1,y1,y2,y3)
Happiness2

fit2<-vglm(cbind(y1,y2,y3)~x1,family=cumulative(parallel = TRUE),data=Happiness2)
summary(Happiness2)

happiness3 <- c(21, 53, 94, 159, 372, 249, 110, 221, 83)
happiness3.table <- as.table(matrix(happiness3, nrow = 3, byrow = FALSE, 
                                    dimnames = list(Income= c(3, 2, 1), 
                                                    Happiness = c('Not too Happy', 'Pretty Happy','Very Happy'))))
happiness3.table

fit3<-vglm(happiness3~Income,family=cumulative(parallel = TRUE),data=happiness3.table)
summary(fit3)

##actual
Happiness[,1]=c(3,2,1) #before , means row and after means column
#OR
IncomeC=c("Average"=3,"Average"=3,"Average"=1)

fit4=vglm(cbind(y1,y2,y3)~IncomeC,family = cumulative(parallel = TRUE),data=Happiness2)
summary(fit4)

#The estimated odds that the person with income increases
#is less happy is 0.572 times the estimated odds is more happy with income




#income increases by 1 level the oddds that
#the person is less happy or less happy direction rather than happy diraecion
#multiplies by 0.5




#activity 1
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


#activity 2
attach(Students)
head(Students)
library(car)
model_null=glm(abor ~ 1, family=binomial, data=Students)#null model 
summary(model_null)
anova(model1,model_null,test = "LRT")
model1 <- glm(abor ~ ideol, family=binomial, data=Students) # link=logit is default
summary(model1)
anova(model_null,model1,test = "LRT")
model2 <- glm(abor ~ relig, family=binomial, data=Students) # link=logit is default
summary(model2)
anova(model_null,model2,test = "LRT")
model3 <- glm(abor ~ news, family=binomial, data=Students) # link=logit is default
summary(model3)
anova(model_null,model3,test = "LRT")
model4 <- glm(abor ~ hsgpa, family=binomial, data=Students) # link=logit is default
summary(model4)
anova(model_null,model4,test = "LRT") ### Model4 not good  (hsgpa)
model5 <- glm(abor ~ gender, family=binomial, data=Students) # link=logit is default
summary(model5)
anova(model_null,model5,test = "LRT") ###Model 5 not good (gender)
model6 <- glm(abor ~ideol+relig+news, family=binomial, data=Students) # link=logit is default
summary(model6)
anova(model_null,model6,test = "LRT")
model7 <- glm(abor ~ideol+relig, family=binomial, data=Students) # link=logit is default
summary(model7)
anova(model7,model6,test = "LRT") 
model8 <- glm(abor ~ideol+news, family=binomial, data=Students) # link=logit is default
summary(model8)
anova(model8,model6,test = "LRT") #remove relig
model9 <- glm(abor ~ideol+news+hsgpa, family=binomial, data=Students) # link=logit is default
summary(model9)
anova(model8,model9,test = "LRT") # ideal model ideol+news+hsgpa
model99 <- glm(abor ~ideol+news+hsgpa+gender, family=binomial, data=Students) # link=logit is default
summary(model9)
anova(model9,model99,test = "LRT") # 
model10 <- glm(abor ~ideol+news+hsgpa+news*ideol, family=binomial, data=Students) # link=logit is default
summary(model10)
anova(model9,model10,test = "LRT")
model11 <- glm(abor ~ideol+news+hsgpa+hsgpa*ideol, family=binomial, data=Students) # link=logit is default
summary(model10)
anova(model9,model11,test = "LRT")
model12 <- glm(abor ~ideol+news+hsgpa+hsgpa*news, family=binomial, data=Students) # link=logit is default
summary(model10)
anova(model9,model12,test = "LRT")


library(MASS)
stepAIC(Model1) # stepwise backward selection using AIC

library(leaps)
library(bestglm)
Students1=data.frame(ideol, news, hsgpa, abor)
bestglm(Students1, family=binomial, IC="BIC") # can also use IC="BIC"


Model1 <- glm(abor ~ideol+news+hsgpa, family=binomial, data=Students)
summary(Model1)
Model2 <- glm(abor ~ideol+news, family=binomial, data=Students)
summary(Model2)


library(pROC)
rocplot1 <- roc(abor ~ fitted(Model1), data=Students)
rocplot2 <- roc(abor ~ fitted(Model2), data=Students)
A1=auc(rocplot1) 
A1
A2=auc(rocplot2)
A2
plot.roc(rocplot1, legacy.axes=TRUE, col="red")
plot.roc(rocplot2, legacy.axes=TRUE, col="blue", add = TRUE)
legend("topleft",legend=c("Model1","Model2"),col=c("red","blue"),lty=1:1)


plot(jitter(abor, 0.5) ~  news, data=Students,ylab="Prob(abor)", xlab = "news", 
     col="blue", ylim = c(-0.1,1.5), xlim = c(0,14), pch=16)

curve(predict(Model2, data.frame(news=x,ideol=c(1)), 
              type="resp"), add=TRUE, col="red")
curve(predict(Model2, data.frame(news=x,ideol=c(2)), 
              type="resp"), add=TRUE, col="blue")
curve(predict(Model2, data.frame(news=x,ideol=c(3)), 
              type="resp"), add=TRUE, col="green")
curve(predict(Model2, data.frame(news=x,ideol=c(4)), 
              type="resp"), add=TRUE, col="pink")
curve(predict(Model2, data.frame(news=x,ideol=c(5)), 
              type="resp"), add=TRUE, col="purple")
curve(predict(Model2, data.frame(news=x,ideol=c(6)), 
              type="resp"), add=TRUE, col="brown")
curve(predict(Model2, data.frame(news=x,ideol=c(7)), 
              type="resp"), add=TRUE, col="yellow")

legend("topleft",c("ideology 1","ideology 2","ideology 3","ideology 4","ideology 5","ideology 6","ideology 7"), 
       lty=c(1, 1), col=c("red", "blue","green","pink","purple","brown","yellow"), cex=0.7)

Students <- read.csv("~/Desktop/DANA-Qualt/Activity_2/Students.dat", sep="")
View(Students)
attach(Students)

model1<-glm(abor~ideol+relig+news+hsgpa+gender, family = binomial, data=Students)
summary(model1)

G=model1$null.deviance-model1$deviance
G
df=model1$df.null-model1$df.residual
df
pvalue=pchisq(G,df,lower.tail = FALSE)
pvalue

Anova(model1)

model_null=glm(abor~1, family = binomial, data=Students)
summary(model_null)
anova(model_null,model1,test = 'LRT')

model1_2=glm(abor~ideol, family = binomial, data=Students)
anova(model_null,model1_2,test = 'LRT')

model1_3=glm(abor~relig, family = binomial, data=Students)
anova(model_null,model1_3,test = 'LRT')

model1_4=glm(abor~news, family = binomial, data=Students)
anova(model_null,model1_4,test = 'LRT')

model1_5=glm(abor~hsgpa, family = binomial, data=Students)
anova(model_null,model1_5,test = 'LRT')
#not good hsgpa

model1_6=glm(abor~gender, family = binomial, data=Students)
anova(model_null,model1_6,test = 'LRT')
#gender not good

model1_7=glm(abor~ideol+relig+news, family = binomial, data=Students)
anova(model_null,model1_7,test='LRT')
#above model is selected

model1_8=glm(abor~relig+news, family = binomial, data=Students)
anova(model1_7,model1_8,test='LRT')

model1_9=glm(abor~ideol+news, family = binomial, data=Students)
anova(model1_7,model1_9,test='LRT')

model1_10=glm(abor~news+ideol+hsgpa, family = binomial, data=Students)
anova(model1_9,model1_10,test='LRT')
#above is ideal model now

model1_11=glm(abor~news+ideol+hsgpa+news*ideol, family = binomial, data=Students)
anova(model1_10,model1_11,test='LRT')

model1_12=glm(abor~news+ideol+hsgpa+news*hsgpa, family = binomial, data=Students)
anova(model1_10,model1_12,test='LRT')

model1_13=glm(abor~news+ideol+hsgpa+ideol*hsgpa, family = binomial, data=Students)
anova(model1_10,model1_13,test='LRT')

#ideal model is model1_10

##AIC
-2*logLik(model1_10)
AIC(model1_10)

install.packages("MASS")
library(MASS)
stepAIC(model1_10)

install.packages("bestglm")
library(leaps)
library(bestglm)
Students1=data.frame(ideol,news,hsgpa,abor)
bestglm(Students1,family=binomial,IC="BIC")


