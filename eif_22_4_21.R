
#-----#
# libraries ----- 
#pac<-c('SuperLearner','caret','glmnet','randomForest','lava','data.table','riskRegression',
#     'Publish','nnet','LogicReg')
#install.packages(c('riskRegression','parallel','foreach','Hmisc',
#      'Publish','nnet','LogicReg','doParallel','acepack'))
#install.packages(pac)

library(SuperLearner)
library(data.table)
library(lava)
library(Hmisc)
library(ggplot2)
library(riskRegression)
library(prodlim)
library(Publish)
library(foreach)
library(parallel)
library(patchwork)
library(doParallel)

options(scipen=999)

################################### M1 simple data set #######################################
# Y0 risk of death among patients that say chest pain
# Y1 risk of death among patients that do not say chest pain
# A: 0=chest pain, 1=atypical symptoms
# M0: probability of ambulance among chest pain sayers
# M1: probability of ambulance among atypical symptom sayers
#Create empty object
m1<-lvm()
# Varibles and distributions
distribution(m1,~Y0) <-binomial.lvm()
distribution(m1,~Y1) <-binomial.lvm()

distribution(m1,~A) <-binomial.lvm(p=0.76)#65% with chest pain
distribution(m1,~Z0) <-binomial.lvm()#72% received emergency ambulance
distribution(m1,~Z1) <-binomial.lvm()
distribution(m1,~Z) <-binomial.lvm()
distribution(m1,~Sex) <-binomial.lvm(p=0.65)# sex: 65% male 
distribution(m1,~Age) <-normal.lvm()# age # prøv at fjerne .mean=67.56, sd=13.6
distribution(m1,~EMS) <-binomial.lvm(p=0.65)#65% called 1-1-2

###Log regression estimation of treatment based on real data
regression(m1) <- Age~71.7-5.8*Sex 
regression(m1) <- A~2.5+0.44*Sex-0.024*Age

# regression on M0 and M1
regression(m1) <- Z0~-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.8*0 #har ikke brystsmerter
regression(m1) <- Z1~-1.5*0.07*Sex+0.0004*Age+1.85*EMS+2.8*1 #har brystsmerter
regression(m1) <- Z~Z1*A+Z0*(1-A)

regression(m1) <- Y0~ -8-1.36*A+0.13*Sex+0.075*Age+0.27*Z0#har fået ambulance som ikke brystsmerter
regression(m1) <- Y1~ -8-1.36*A+0.13*Sex+0.075*Age+0.27*Z1#har fået ambulance som brystsmerter

plot(m1)
print(m1) 

##---------------------##
### True parameter ----
set.seed(123); d<- sim(m1,1e7) 
setDT(d)

#true parameter
(true.psi1 <- mean(d[A==0,Y1]))
(true.psi2 <- mean(d[A==0,Y0]))
true.psi<-true.psi1-true.psi2;true.psi

##-----------------##

#-- sample size
n <- 5000

# define SL libraries ----
sl.lib.mediator <- c('SL.glm','SL.glm.interaction')
sl.lib.outcome <- c('SL.glm','SL.glm.interaction')
sl.lib.exposure <- c('SL.glm','SL.glm.interaction')

#-- estimators
psi1 <- list()
psi2 <- list()

#-- estimator of the difference
psi <- list()

#-- to store eif
eif <- list()

#-- to store se estimators
se1 <- list()
se2 <- list()

#-- to store se of the difference
se <- list()

for (i in 1:500) {
  set.seed(i+123)
  
  d<- sim(m1,n) 
  #combine outcomes/mediator for A=1 and A=0 to one
  setDT(d)
  d[A==1,Z:=Z1]
  d[A==0,Z:=Z0]
  d[A==1,Y:=Y1]
  d[A==0,Y:=Y0]
  
  dat<-copy(d[,.(id=1:.N,
             A=as.numeric(A),
             Z=as.numeric(Z),
             EMS=EMS,
             Age=Age,
             Sex=factor(Sex),
             Y=as.numeric(Y))])
  
  # prepare data ----
  setDT(dat)
  
  # Target parameter----
  #-------------------------------------------------------------------------------------------------#
  #exposure-model for all patients
  pifit<-SuperLearner(Y=dat[,as.numeric(A==1)],
                      X=dat[,.(Sex,Age)],
                      family = binomial(),
                      SL.library = sl.lib.exposure)
  
  #outcome-model for all patients
  Q.fit<-SuperLearner(Y=dat[,as.numeric(Y==1)],
                      X=dat[,.(A,Sex,Age,Z)],
                      family = binomial(),
                      SL.library = sl.lib.outcome)
  
  # distribution of ambulance for all patients ----
  gammafit<-SuperLearner(Y=dat[,Z],
                         X=dat[,.(A,Sex,Age,EMS)],
                         family = binomial(),
                         SL.library = sl.lib.mediator)
  
  
  pibar <- dat[,mean(A==0)]
  
  # predict chance of receiving an ambulance among non-chest pain patients and chest pain patients using 
  # distripution from chest pain patients
  dat[,gammahat.a1:=predict(gammafit, newdata =dat[,.(EMS,Sex,Age,A=1)],onlySL = T)$pred]
  dat[,gammahat.a0:=predict(gammafit, newdata =dat[,.(EMS,Sex,Age,A=0)],onlySL = T)$pred]
  
  # probability of having chest pain
  dat[,pihat:=predict(pifit, newdata =dat[,.(Sex,Age)],onlySL = T)$pred]
  
  # assign intervention with ambulance to all patients - all patients both receive ambulance and does not receive ambulance
  #data is now twice as long (as all observations contribute with two rows)
  
  expanded.data<-data.table(rbind(copy(dat)[,Z:=0],
                                  copy(dat)[,Z:=1]),
                            Z.obs=c(dat[, Z]))
  
  setkey(expanded.data,id)
  
  # Estimated Y for all
  expanded.data[,Qhat:=predict(Q.fit, newdata = expanded.data[,.(Sex,Age,Z,A)],onlySL = T)$pred]
  
  # Estimated Y when no one had chest pain
  #expanded.data[,Qhat.a1:=predict(Q.fit, newdata = expanded.data[,.(Sex,Age,Z,A=1)],onlySL = T)$pred]
  # Estimated Y when all had chest pain
  expanded.data[,Qhat.a0:=predict(Q.fit, newdata = expanded.data[,.(Sex,Age,Z,A=0)],onlySL = T)$pred]
  
  
  # Estimated mortality when patients without chest pain had the same probability of receiving ambulance as patients with chest pain
  expanded.data[,psi_1:= sum(Qhat.a0*(gammahat.a1*Z+(1-gammahat.a1)*(1-Z))),id]
  # Estimated mortality when patients without chest pain received ambulance as observed
  expanded.data[,psi_2:= sum(Qhat.a0*(gammahat.a0*Z+(1-gammahat.a0)*(1-Z))),id]
  
  #-------------------------------------------------------------------------------------------------#
  observed.data<-expanded.data[Z==Z.obs]
  # Psi 1 ----
  psi1[[i]] <- psihat_1 <- observed.data[,(1/pibar)*mean((A==0)*psi_1)]
  psi2[[i]] <- psihat_2 <- observed.data[,(1/pibar)*mean((A==0)*psi_2)]
  
  psi[[i]] <- psihat <- psihat_1-psihat_2
  
  # influence function ----
  #-------------------------------------------------------------------------------------------------#
  eif_psi1<-observed.data[,(((Z*gammahat.a1+(1-Z)*(1-gammahat.a1))/
                              (Z*gammahat.a0+(1-Z)*(1-gammahat.a0)))*((A==0)/pibar)*
                                       (as.numeric(Y=='1') - Qhat.a0) +  # skal det ikke være Qhat her? - det giver det samme på grund af indicator function
                                       (A==1)/pihat*(1-pihat)/pibar*(Qhat.a0 - psi_1) +
                                       (A==0)/pibar*(psi_1 - psihat_1))]
  
  eif_psi2<-observed.data[,((A==0)/pibar*(as.numeric(Y=='1') - Qhat.a0) +
                                       (A==0)/pibar*(Qhat.a0 - psi_2) + 
                                       (A==0)/pibar*(psi_2 - psihat_2))]

  
  eif[[i]] <- eif_psi1-eif_psi2
  
  se1[[i]] <- sqrt(mean(eif_psi1^2)/nrow(observed.data))
  se2[[i]] <- sqrt(mean(eif_psi2^2)/nrow(observed.data))
  se[[i]] <- sqrt(mean((eif_psi1-eif_psi2)^2)/nrow(observed.data))

  print(i) 
}



#how often is the true parameter within the estimated confidence interval (the coverage)
cov1 <- mean(unlist(psi1)-1.96*unlist(se1)<=true.psi1 & unlist(psi1)+1.96*unlist(se1)>=true.psi1)

cov2 <- mean(unlist(psi2)-1.96*unlist(se2)<=true.psi2 & unlist(psi2)+1.96*unlist(se2)>=true.psi2)

cov <- mean(unlist(psi)-1.96*unlist(se)<=true.psi & 
              unlist(psi)+1.96*unlist(se)>=true.psi)

cov1;cov2;cov


# how close to the true parameter are we?theme_set(theme_classic())
dat.psi<-as.data.frame(cbind(psi1=unlist(psi1),psi2=unlist(psi2),psi=unlist(psi)))
plot.psi1<-ggplot(dat.psi)+
  geom_histogram(aes(psi1),fill='slategrey',bins = 10)+
  geom_vline(xintercept=as.numeric(true.psi1), color='orange')
plot.psi2<-ggplot(dat.psi)+
  geom_histogram(aes(psi2),fill='slategrey', bins=10)+
  geom_vline(xintercept=as.numeric(true.psi2), color='orange')
plot.psi<-ggplot(dat.psi)+
  geom_histogram(aes(psi),fill='slategrey', bins=10)+
  geom_vline(xintercept=as.numeric(true.psi), color='orange')

plot.psi1+plot.psi2+plot.psi


mean(unlist(psi))
mean(unlist(psi1))
mean(unlist(psi2))

mean(unlist(se))
mean(unlist(se1))
mean(unlist(se2))




