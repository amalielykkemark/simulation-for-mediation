#-----#
# libraries ----- 
#pac<-c('SuperLearner','caret','glmnet','randomForest','lava','data.table','riskRegression',
#     'Publish','nnet','LogicReg')
#install.packages(c('riskRegression','parallel','foreach','Hmisc',
#      'Publish','nnet','LogicReg','doParallel','acepack'))
#install.packages(pac)

library(SuperLearner)
library(randomForest)
library(data.table)
library(lava)

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

distribution(m1,~A) <-binomial.lvm(p=0.35)#35% without chest pain
distribution(m1,~M0) <-binomial.lvm()#72% received emergency ambulance
distribution(m1,~M1) <-binomial.lvm()
distribution(m1,~Sex) <-binomial.lvm(p=0.65)# sex: 65% male 
distribution(m1,~Age) <-normal.lvm()# age # prøv at fjerne .mean=67.56, sd=13.7

###Log regression estimation of treatment based on real data
regression(m1) <- Age~71.367-5.9*Sex 
regression(m1) <- A~-1.16-0.33*Sex+0.011*Age

# regression on M0 and M1
regression(m1) <- M0~0.95+0.28*Sex+0.014*Age
regression(m1) <- M1~-0.67+0.05*Sex+0.003*Age

#regression on Y A=0
regression(m1) <- Y0~ -10.51+0.27*Sex+0.09*Age+0.15*M0
#regression on Y A=1
regression(m1) <- Y1~ -7.33+0.075*Sex+0.067*Age+0.092*M1


plot(m1)
print(m1) 

set.seed(1); d<- sim(m1,5000) # evt 100.000 eller 10x10.000
# tjek at regressioner giver samme resultat ca. 
setDT(d)
d[A=='1',M:=M1]
d[A=='0',M:=M0]
d[A=='1',Y:=Y1]
d[A=='0',Y:=Y0]

d[,.(id=1:.N,
     A=factor(A),
     M=factor(M),
     Age=Age,
     Sex=factor(Sex),
     Y=Y)]

fit <- glm(Y~A+M+Sex+Age, family = binomial(),data=d)# (til TAG: jeg får nogenlunde samme værdier nu)


################################### estimate NIE super simple (A,M,Y) #######################################

dat<-d[,.(A,M,Y)]

mediation.effect<-function(data,A,M,Y) {
  #data<-copy(d)
  datA1<-copy(data[A==1])
  datA0<-copy(data[A==0])
  
  #probability of receiving ambulance
  # among patients with atypical symptoms (A=1)
  p.M1A1<-datA1[M==1,.N]/datA1[,.N]
  # among patients with chest pain (A=0)
  p.M1A0<-datA0[M==1,.N]/datA0[,.N]
  
  # probability of 30day mortality for those receiving ambulance (M1) among atypical symptoms (A=1)
  p.Y1A1M1<-datA1[M==1 & Y==1,.N]/datA1[M==1,.N]
  # probability of 30day mortality for those not receiving ambulance (M0) among atypical symptoms (A=1)
  p.Y1A1M0<-datA1[M==0 & Y==1,.N]/datA1[M==0,.N]
  
  #risk of death if atypical symptoms received ambulance as they did
  p.no.intervention<-p.Y1A1M1*p.M1A1+p.Y1A1M0*(1-p.M1A1)
  
  #risk of death if atypical symptoms received ambulance as often as patients with chest pain
  p.intervention<-p.Y1A1M1*p.M1A0+p.Y1A1M0*(1-p.M1A0)
  nie<-p.no.intervention-p.intervention
    
  
  return(list(no.intervention=p.no.intervention,
              intervention=p.intervention,
              nie=nie))
  }

mediation.effect(data=dat,A=A,M=M,Y=Y)

################################### estimate NIE simple (A,M,Y,Sex) #######################################
dat<-d[,.(A,M,Y,Sex)]

mediation.effect.x<-function(data,A,M,Y,X) {
  data<-copy(dat)
  data[,X:=factor(Sex)]
  datA1<-copy(data[A==1])
  datA0<-copy(data[A==0])
  
  #probability of receiving ambulance
  # among patients with atypical symptoms (A=1)
  p.M1A1X0<-datA1[X==0 & M==1,.N]/datA1[X==0,.N]
  p.M1A1X1<-datA1[X==1 & M==1,.N]/datA1[X==1,.N]
  # among patients with chest pain (A=0)
  p.M1A0X0<-datA0[M==1&X==0,.N]/datA0[X==0,.N]
  p.M1A0X1<-datA0[M==1&X==1,.N]/datA0[X==1,.N]
  
  # probability of 30day mortality for those receiving ambulance (M1) among atypical symptoms (A=1)
  p.Y1A1M1X0<-datA1[M==1 & Y==1 & X==0,.N]/datA1[M==1 & X==0,.N]
  p.Y1A1M1X1<-datA1[M==1 & Y==1 & X==1,.N]/datA1[M==1 & X==1,.N]
  # probability of 30day mortality for those not receiving ambulance (M0) among atypical symptoms (A=1)
  p.Y1A1M0X0<-datA1[M==0 & Y==1 & X==0,.N]/datA1[M==0 & X==0,.N]
  p.Y1A1M0X1<-datA1[M==0 & Y==1 & X==1,.N]/datA1[M==0 & X==1,.N]
  
  #risk of death if atypical symptoms received ambulance as they did
  p.no.interventionX0<-p.Y1A1M1X0*p.M1A1X0+p.Y1A1M0X0*(1-p.M1A1X0)
  p.no.interventionX1<-p.Y1A1M1X1*p.M1A1X1+p.Y1A1M0X1*(1-p.M1A1X1)
  
  #risk of death if atypical symptoms received ambulance as often as patients with chest pain
  p.interventionX0<-p.Y1A1M1X0*p.M1A0X0+p.Y1A1M0X0*(1-p.M1A0X0)
  p.interventionX1<-p.Y1A1M1X1*p.M1A0X1+p.Y1A1M0X1*(1-p.M1A0X1)
  
  
  nie.X0<-p.no.interventionX0-p.interventionX0
  nie.X1<-p.no.interventionX1-p.interventionX1
  
  return(list(nie.X0=nie.X0,
              nie.X1=nie.X1))
}

mediation.effect.x(data=d,A=A,M=M,Y=Y,X=Sex)


