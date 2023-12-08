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




