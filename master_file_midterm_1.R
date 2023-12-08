#Master file for Quanlitative midterm 1
#Chapter 1
prop.test(837, 1810, p=0.5, correct = FALSE)

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

#Chapter 2

prop.test(c(189,104),c(11034,11037),conf.level = 0.95,correct = FALSE)

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

#death penalty
#create a data frame
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

#physicians test
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

#political data
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
Evolution <- read.csv("~/Desktop/DANA-Qualt/Chapter 3/Evolution.dat", sep="")
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


#heart2
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



#logistic regression curve


x <- seq(0,20, length=100)


pi=exp(-5+0.7*x)/(1+exp(-5+0.7*x))
plot(x, pi, type="l", lty=1, xlab="x",
     ylab="π(x)", main="Logistic regression functions", col="green", ylim = c(0,1))
pi2= exp(5-0.7*x)/(1+exp(5-0.7*x))
lines(x, pi2, col="red", lty=1)

legend("topright",c("β>0","β<0"), lty=c(1, 1), col=c("green", "red"))



c(phat-qnorm(0.025,lower.tail = FALSE)*SE,phat+qnorm(0.025,lower.tail = FALSE)*SE)

A=CMHtest(smoke.table,rscores=c(5,4,3,2,1),cscores=1:0,types='cor')
n=sum(smoker.table)
M=sqrt(A$table[1])
R=M/sqrt(n-1) #correlation