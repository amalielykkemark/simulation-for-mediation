
#-----#
# libraries ----- 
pac<-c('SuperLearner','caret','glmnet','randomForest','lava','data.table','riskRegression',
       'Publish','nnet','LogicReg','doParallel','acepack')
install.packages(pac)

library(SuperLearner)
library(caret)
library(glmnet)
library(randomForest)
library(data.table)
library(lava)
library(gam)
library(nnet)
library(rpart)
library(e1071)
library(LogicReg)
library(riskRegression)
library(prodlim)
library(Publish)
library(doParallel)
library(igraph)


options(scipen=999)

################################### M1 simple data set #######################################
## OBS find ud af hvorfor det ikke passer???
#Create empty object
m1<-lvm()
# Varibles and distributions
distribution(m1,~Y0) <-binomial.lvm()
distribution(m1,~Y1) <-binomial.lvm()
distribution(m1,~A) <-binomial.lvm(p=0.35)#35% without chest pain
distribution(m1,~M) <-binomial.lvm(p=0.72)#72% received emergency ambulance

distribution(m1,~W1) <-binomial.lvm(p=0.3)# 30% with previous MI
distribution(m1,~W2) <-binomial.lvm(p=0.13)# 13% with HF
m1<- categorical(m1,~W3, K=3, p=c(0.59,0.08), labels=c(0,1,2))# education
transform(m1,W3_0~W3) <-function(x) {1*(x==0)}
transform(m1,W3_1~W3) <-function(x) {1*(x==1)}
transform(m1,W3_2~W3) <-function(x) {1*(x==2)}
distribution(m1,~W4) <-binomial.lvm(p=0.65)# sex: 65% male
distribution(m1,~W5) <-normal.lvm(mean=67.56, sd=13.7)# age

###Log regression estimation of treatment based on real data
regression(m1) <- A~ -1.12-0.59*W1+0.23*W2-0.14*W3_1-0.12*W3_2-0.29*W4 + 0.013*W5
regression(m1) <- M~1.46-2.49*A+0.18*W1-0.21*W2-0.02*W3_1-0.29*W3_2+0.16*W4+0.007*W5
#regression on Y only for A=1
regression(m1) <- Y1~ -7.24-0.14*W1 + 0.22*W2 - 0.08*W3_1-0.08*W3_2 + 0.95*W4 + 0.066*W5+ 0.1*M
#regression on Y only for A=0
regression(m1) <- Y0~ -10.28-0.28*W1 + 0.69*W2 - 0.09*W3_1-0.08*W3_2 + 0.28*W4 + 0.088*W5+ 0.17*M

plot(m1)
print(m1) 

##############################################
########### simulate data from m1 ############

set.seed(1); dat<- sim(m1,5000) 
dat$Y<-(dat$Y1*dat$A)+(dat$Y0*(1-dat$A))

table(dat$A,dat$Y)

True_EY.0<- mean(dat$Y0)
True_EY.1<- mean(dat$Y1)
True_ATE<-True_EY.1-True_EY.0;True_ATE
True_OR<- (True_EY.1*(1-True_EY.0))/((1-True_EY.1)*True_EY.0);True_OR


##############################################
########### simulate data from m1 ############

sl.lib<-c('SL.glmnet','SL.mean','SL.glm','SL.randomForest','SL.glm.interaction')

# tilpas nogle algoritmer ... og tilføj dem til sl.lib

#--------------------------------------------------------------------------------------------#
########### simulate data from m1 ############

d<- sim(m1,5000) 
setDT(d)
dat$Y<-(dat$Y1*dat$A)+(dat$Y0*(1-dat$A))
d[,Y:= (Y1*A)+(Y0*(1-A))]
d<-d[,.(mi=W1,hf=W2,edu=W3,sex=W4,age=W5,symptom=factor(A),ambulance=M,Y)]

samp<-sample(nrow(d),4000)
# create training set
x.train <-d[samp]
# create validation set
hold.out.set<-d[-samp]
# create validation set for outcome
y.hold.out.set <- d[-samp,Y]


########### split data into exposed and unexposed ############
dBryst <- x.train[symptom==1]
dOther <- x.train[symptom==0]
  
########### mediation analysis using glm ############

# ambulance models
amBryst <- glm(ambulance~age+sex+edu+hf+mi,data=dBryst,family="binomial")
amOther <- glm(ambulance~age+sex+edu+hf+mi,data=dOther,family="binomial")
  
# outcomme models
mortBryst <- glm(Y~age+sex+edu+hf+mi+ambulance,data=dBryst,family="binomial")
mortOther <- glm(Y~age+sex+edu+hf+mi+ambulance,data=dOther,family="binomial")
  
# predict ambulance bland other i hypotetisk setting hvor de fik
# en ambulance lige saa hyppig som dem med Brystsmerter
dOther.hyp <- copy(dOther)
dOther.hyp[,symptom:=1]
p.am <- predictRisk(amBryst,newdata=dOther.hyp)

# evaluate change in outcome in hypothetical setting
loopresult <- foreach(b=1:20,.combine="rbind") %dopar% {
  # new variable AmbulanceB represents if patient has received
  # ambulance in the hypothetical situation
  hyp.other <- copy(dOther)
  hyp.other[,ambulance:=factor(rbinom(.N,1,p.am))]
  predictRisk(mortOther,newdata=hyp.other)
  }
  
result <- apply(loopresult,2,mean)
c(observed=dOther[,mean(Y)],hypothetical=mean(result))


########### mediation analysis using super learner ############

# create training set for outcome
y.train.other <- d[samp][symptom==1,Y]
y.train.chest <- d[samp][symptom==0,Y]
# create training set for mediator
m.train.other <- d[samp][symptom==1,ambulance]
m.train.chest <- d[samp][symptom==0,ambulance]

# fit the super learner for mediator among  patients with chest pain
sup.m<-SuperLearner(Y=m.train.chest,
               X=dBryst,
               family = binomial(),
               SL.library = sl.lib)

# fit the super learner for outcome among patients without chest pain
sup.y<-SuperLearner(Y=y.train.other,
                    X=dOther,
                    family = binomial(),
                    SL.library = sl.lib)
  
# predict ambulance bland other i hypotetisk setting hvor de fik
# en ambulance lige saa hyppig som dem med Brystsmerter
dOther.hyp <- copy(dOther)
dOther.hyp[,symptom:=1]
p.am <- predictRisk(sup.m,newdata=dOther.hyp)

# evaluate change in outcome in hypothetical setting
loopresult <- foreach(b=1:20,.combine="rbind") %dopar% {
  # new variable AmbulanceB represents if patient has received
  # ambulance in the hypothetical situation
  hyp.other <- copy(dOther)
  hyp.other[,ambulance:=factor(rbinom(.N,1,p.am))]
  predictRisk(mortOther,newdata=hyp.other)
}

result <- apply(loopresult,2,mean)
c(observed=dOther[,mean(Y)],hypothetical=mean(result))


pre<-predict(sup,hold.out.set, onlySL = T)
  
summary(pre$library.predict)




