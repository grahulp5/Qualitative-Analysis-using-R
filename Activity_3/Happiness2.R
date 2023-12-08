#Is income significance for happiness, for regression? 
#Fit the regression line, to see weather income is significant for happines

#Treat income as categorical variable

NotTooHappy = c(21,53,94)
PrettyHappy = c(159,372,249)
VeryHappy = c(110,221,83)
Income = c("AboveAverage","average","Belowaverage")
Happiness =data.frame(Income,NotTooHappy,PrettyHappy,VeryHappy)
library(VGAM)
attach(Happiness)
fit_happy <- vglm(cbind(NotTooHappy,PrettyHappy,VeryHappy) ~ factor(Income),family=cumulative(parallel=TRUE), data=Happiness)
# "parallel=TRUE" imposes proportional odds structure
summary(fit_happy)
#For any fixed j the estimated odds that a family with an average income
#is in the not happines direction rather than the happiness direction equal 1.163695 times the estimated odds for a family with an income above average.

# Treat income as ordinal

IncomeC = c("AboveAverage"=3,"Average"=2,"Belowaverage"=1)
Happiness =data.frame(IncomeC,NotTooHappy,PrettyHappy,VeryHappy)
fit_happy2 <- vglm(cbind(NotTooHappy,PrettyHappy,VeryHappy) ~ IncomeC,family=cumulative(parallel=TRUE), data=Happiness)
# "parallel=TRUE" imposes proportional odds structure
summary(fit_happy2)


y1 = c(37,25,6)
y2 = c(90,93,18)
y3 = c(45,56,13)
Inc = c("Belowaverage"=1,"Average"=2,"AboveAverage"=3)
Happiness3 = data.frame(Inc,y1,y2,y3)
fit_happy3<- vglm(cbind(y1,y2,y3) ~ Inc,family=cumulative(parallel=TRUE), data=Happiness3)
# "parallel=TRUE" imposes proportional odds structure
summary(fit_happy3)
coef(fit_happy3)
fit_happy3
null_fit <- vglm(cbind(y1,y2,y3)~ 1,family=cumulative(parallel=TRUE), data=Happiness3)
lrtest(fit_happy3,null_fit)
exp(-4.07)