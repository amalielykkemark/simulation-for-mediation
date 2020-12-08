
#-----#
# libraries ----- 
pac<-c('SuperLearner','caret','glmnet','randomForest','lava','data.table','riskRegression',
<<<<<<< HEAD
       'Publish','nnet','LogicReg')
install.packages(c('riskRegression','parallel','foreach','Hmisc'))
=======
       'Publish','nnet','LogicReg','doParallel','acepack')
install.packages(pac)
>>>>>>> b3c7bc06a36ea45a7490f0621aa711f9d9ae1c62

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
library(Hmisc)
library(riskRegression)
library(prodlim)
library(Publish)
<<<<<<< HEAD
library(foreach)
library(parallel)
=======
library(doParallel)
library(igraph)

>>>>>>> b3c7bc06a36ea45a7490f0621aa711f9d9ae1c62

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
m1<- categorical(m1,~W3, K=3, p=c(0.33,0.59,0.08), labels=c(0,1,2))# education
transform(m1,W3_0~W3) <-function(x) {1*(x==0)}
transform(m1,W3_1~W3) <-function(x) {1*(x==1)}
transform(m1,W3_2~W3) <-function(x) {1*(x==2)}
distribution(m1,~W4) <-binomial.lvm(p=0.65)# sex: 65% male
distribution(m1,~W5) <-normal.lvm(mean=67.56, sd=13.7)# age

###Log regression estimation of treatment based on real data
regression(m1) <- A~ -1.12-0.59*W1+0.23*W2-0.14*W3_1-0.12*W3_2-0.29*W4 + 0.013*W5
regression(m1) <- M~1.46-2.49*A+0.18*W1-0.21*W2-0.02*W3_1-0.29*W3_2+0.16*W4+0.007*W5
#regression on Y
#regression(m1) <- Y~ -9.12+1.16*A-0.20*W1+0.44*W2-0.09*W3_1-0.07*W3_2+0.17*W4+0.007*W5+0.12*M

#regression on Y only for A=1
regression(m1) <- Y1~ -7.24-0.14*W1 + 0.22*W2 - 0.08*W3_1-0.08*W3_2 + 0.95*W4 + 0.066*W5+ 0.1*M
#regression on Y only for A=0
regression(m1) <- Y0~ -10.28-0.28*W1 + 0.69*W2 - 0.09*W3_1-0.08*W3_2 + 0.28*W4 + 0.088*W5+ 0.17*M

plot(m1)
print(m1) 

##############################################
########### simulate data from m1 ############

set.seed(1); d<- sim(m1,5000) 
setDT(d)
d
d[,Y:= (Y1*A)+(Y0*(1-A))]
True_EY0<- mean(d[A==0,Y])
True_EY1<- mean(d[A==1,Y])
True_ATE<-True_EY1-True_EY0;True_ATE

setnames(d,"M","ambulance")
setnames(d,"W4","sex")
setnames(d,"W5","age")
setnames(d,"W1","pre.mi")
setnames(d,"W2","pre.hf")
setnames(d,"W3","education")

d <- d[,.(Y=Y,
          A=factor(A),
          ambulance=factor(ambulance),
          age,
          sex=factor(sex),
          education,
          pre.mi=factor(pre.mi),
          pre.hf=factor(pre.hf))]
str(d)

samp<-sample(nrow(d),4000)
# create training set
x.train <-d[samp]
# create validation set
hold.out.set<-d[-samp]
# create training set for outcome
y.train <- d[samp,Y]
# create validation set for outcome
y.hold.out.set <- d[-samp,Y]

#split data into to groups
dBryst <- d[A==0]
dOther <- d[A==1]

# ambulance models on the two different datasets
amBryst <- glm(ambulance~age+sex+pre.mi+pre.hf+education,data=dBryst,family="binomial")
amOther <- glm(ambulance~age+sex+pre.mi+pre.hf+education,data=dOther,family="binomial")

# outcomme models on the  two different datasets
mortBryst <- glm(Y~age+sex+pre.mi+pre.hf+education+ambulance,data=dBryst,family="binomial")
mortOther <- glm(Y~age+sex+pre.mi+pre.hf+education+ambulance,data=dOther,family="binomial")

# predict ambulance among other symptoms in a hypothetical setting
# where these patients received an emergency ambulance as often 
# as patients with chest pain
dOther.hyp <- copy(dOther)
dOther.hyp[,A:=1]
p.am <- predictRisk(amBryst,newdata=dOther.hyp)

# evaluate change in outcome in hypothetical setting
loopresult <- foreach(b=1:100,.combine="rbind") %dopar% {
  # new variable ambulance represents if patient has received
  # ambulance in the hypothetical situation
  hyp.other <- copy(dOther)
  hyp.other[,ambulance:=factor(rbinom(.N,1,p.am))]
  #predict the risk of dying when patients without chest pain received
  #an ambulance as often as patients with chest pain
  predictRisk(mortOther,newdata=hyp.other)
}

result <- apply(loopresult,2,mean)
c(observed.oucome=True_EY1,hypothetical=mean(result))



# fit the super learner
sup<-SuperLearner(Y=y.train,
                  X=x.train,
                  family = binomial(),
                  SL.library = sl.lib)

pre<-predict(sup,hold.out.set, onlySL = T)

summary(pre$library.predict)















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
d<-d[,.(mi=W1,hf=W2,edu=W3,sex=W4,age=W5,symptom=factor(A),ambulance=factor(M),M,Y)]

samp<-sample(nrow(d),4000)
# create training set
x.train <-d[samp]
# create validation set
hold.out.set<-d[-samp]
# create validation set for outcome
y.hold.out.set <- d[-samp,Y]

########### split data into exposed and unexposed ############
dBryst <- x.train[symptom==0]
dOther <- x.train[symptom==1]
  
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
dOther.hyp[,symptom:=NULL]
dOther.hyp[,symptom:=0]
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
y.train.other <- dOther[symptom==1,Y]
y.train.chest <- dBryst[symptom==0,Y]

# create training set for mediator
m.train.other <- dOther[symptom==1,M]
m.train.chest <- dBryst[symptom==0,M]

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
dOther.hyp[,symptom:=NULL]
dOther.hyp[,symptom:=0]
p.am <- predict(sup.m,newdata=dOther.hyp,onlySL = T)

# evaluate change in outcome in hypothetical setting
loopresult <- foreach(b=1:20,.combine="rbind") %dopar% {
  # new variable AmbulanceB represents if patient has received
  # ambulance in the hypothetical situation
  hyp.other <- copy(dOther)
  hyp.other[,ambulance:=factor(rbinom(.N,1,p.am))]
  predict(sup.y,newdata=hyp.other,onlySL = T)
}

result <- apply(loopresult,2,mean)
c(observed=dOther[,mean(Y)],hypothetical=mean(result))



