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


