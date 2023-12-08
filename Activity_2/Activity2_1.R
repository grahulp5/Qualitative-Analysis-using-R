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