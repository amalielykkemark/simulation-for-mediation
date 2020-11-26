
#-----#
# libraries ----- 
pac<-c('SuperLearner','caret','glmnet','randomForest','lava','data.table','riskRegression',
       'Publish','nnet','LogicReg')

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

packageVersion("Publish")
packageVersion("lava")
packageVersion("igraph")

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
d<-dat[,.(W1,W2,W3,W4,W5,A,M,Y)]

##############################################
########### simulate data from m1 ############

sl.lib<-c('SL.glmnet','SL.mean','SL.glm','SL.randomForest','SL.glm.interaction')

# tilpas nogle algoritmer ... og tilføj dem til sl.lib


#--------------------------------------------------------------------------------------------#
########### simulate data from mpos ############
runone <- function(i) {
  print(i)
  
  d<- sim(m1,5000) 
  setDT(d)
  dat$Y<-(dat$Y1*dat$A)+(dat$Y0*(1-dat$A))
  d[,Y:= (Y1*A)+(Y0*(1-A))]
  d<-d[,.(W1,W2,W3,W4,W5,A=factor(A),M=factor(M),Y)]
  samp<-sample(nrow(d),4000)
  # create training set
  x.train <-d[samp]
  # create validation set
  hold.out.set<-d[-samp]
  # create training set for outcome
  y.train <- d[samp,Y]
  # create validation set for outcome
  y.hold.out.set <- d[-samp,Y]
  
  
  reg<-glm(Y~A+W1+W2+W3+W4+W5+M,
           data=x.train,
           family=binomial)
  
  riskreg<-ate(reg,
               data=x.train,
               treatment="A",
               se=TRUE)
  
  #### superlearner
  d<-d[,c("W1","W2","W3","W4","A","M","Y")]
  
  # fit the super learner
  sup<-SuperLearner(Y=y.train,
               X=x.train,
               family = binomial(),
               SL.library = sl.lib)
  
  pre<-predict(sup,hold.out.set, onlySL = T)
  
  summary(pre$library.predict)
  
  return(unlist(c(riskreg$riskComparison[,c(4,5,7,8,6)],
                  #obs rediger her
                  super$effect.measures$ATE[c(2,3,5,4)])))
}

#-------------------------------------- end run function ------------------------

Positsimlist <- lapply(1:10, runone)
Positsimmat <- matrix(unlist(Positsimlist), ncol=15, byrow = TRUE)
simmat_nearviolation<-Positsimmat
simmat_nearviolation_mean<-colMeans(Positsimmat)
simmat_nearviolation_mean

#write.table(Positsimmat,"/Users/amalielykkemarkmoller/Desktop/Speciale/R/Resulater/sim_pos_100rep.txt")
#write.table(simmat_nearviolation_mean,"/Users/amalielykkemarkmoller/Desktop/Speciale/R/Resulater/colmeans_sim_pos_100rep.txt")

#---------True ate and or estimated from 40. mio observtaions -------#
TRUEate_p <-0.1271935
TRUEor_p  <-2.074362


#---------True ate and or estimated from 40. mio observtaions -------#
TRUEate_p <-0.1271935
TRUEor_p  <-2.074362

################# Calculate Coverage #####################################################
# CI contains true value ATE (riskreg)
riskregATE1x<-Positsimmat[,3]
riskregATE2x<-Positsimmat[,4]
regCI_95x <- riskregATE1x < TRUEate_p & TRUEate_p < riskregATE2x
summary(regCI_95x)
mean(regCI_95x)

# CI contains true value ATE (tmle)
tmleATE1x<-Positsimmat[,8]
tmleATE2x<-Positsimmat[,9]
tmleCI_95x <- tmleATE1x < TRUEate_p & TRUEate_p < tmleATE2x
summary(tmleCI_95x)
mean(tmleCI_95x)

# CI contains true value ATE (ltmle)
ltmle1ATE1x<-Positsimmat[,13]
ltmle1ATE2x<-Positsimmat[,14]
ltmle1CI_95x <- ltmle1ATE1x < TRUEate_p & TRUEate_p < ltmle1ATE2x
summary(ltmle1CI_95x)
mean(ltmle1CI_95x)

# CI contains true value ATE (ltmle, stratify=TRUE, gbounds=(0.025,0.975))
ltmle2ATE1x<-Positsimmat[,18]
ltmle2ATE2x<-Positsimmat[,19]
ltmle2CI_95x <- ltmle2ATE1x < TRUEate_p & TRUEate_p < ltmle2ATE2x
summary(ltmle2CI_95x)
mean(ltmle2CI_95x)

# CI contains true value ATE (ltmle, gbounds=(0.025,0.975))
ltmle3ATE1x<-Positsimmat[,23]
ltmle3ATE2x<-Positsimmat[,24]
ltmle3CI_95x <- ltmle3ATE1x < TRUEate_p & TRUEate_p < ltmle3ATE2x
summary(ltmle3CI_95x)
mean(ltmle3CI_95x)

# CI contains true value ATE (ltmle, gbounds=(0.05,0.95))
ltmle4ATE1x<-Positsimmat[,28]
ltmle4ATE2x<-Positsimmat[,29]
ltmle4CI_95x <- ltmle4ATE1x < TRUEate_p & TRUEate_p < ltmle4ATE2x
summary(ltmle4CI_95x)
mean(ltmle4CI_95x)

# sd risk reg
ESE1<-sd(Positsimmat[,1]);ESE1
ASE1<-mean(Positsimmat[,2]);ASE1

# sd TMLE
ESE2<-sd(Positsimmat[,6]);ESE2
ASE2<-mean(sqrt(Positsimmat[,7]));ASE2

# sd Ltmle
ESE3<-sd(Positsimmat[,11]);ESE3
ASE3<-mean(Positsimmat[,12]);ASE3

# sd Ltmle (0.025,0.975)
ESE4<-sd(Positsimmat[,16]);ESE4
ASE4<-mean(Positsimmat[,17]);ASE4

# sd Ltmle (0.025,0.975) Stratify=TRUE
ESE5<-sd(Positsimmat[,21]);ESE5
ASE5<-mean(Positsimmat[,22]);ASE5

# sd Ltmle (0.05,0.95) 
ESE6<-sd(Positsimmat[,26]);ESE6
ASE6<-mean(Positsimmat[,27]);ASE6



