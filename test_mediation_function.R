
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
library(riskRegression)
library(prodlim)
library(Publish)
library(foreach)
library(parallel)
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
#combine outcomes/mediator for A=1 and A=0 to one
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

# tjek at regressioner giver samme resultat ca. 
glm(Y~A+M+Sex+Age, family = binomial(),data=d)# (til TAG: jeg får nogenlunde samme værdier nu)

##############################################
########### simulate data from m1 ############

ambulance<-function(data, A, n.train, n, model, formula.m, formula.y, sl.lib, true.parameters){
  #data<-d
  #split data into training and validation set
  setDT(data)
  train<-data[1:n.train]
  validation<-data[(n.train+1):n]
  #split data into two groups
  dBryst <- train[A==0]
  dOther <- train[A==1]
  #split data into two groups
  dBryst.val <- validation[A==0]
  dOther.val <- validation[A==1]
  
  #vælg mellem sl eller glm med if statement
  if (true.parameters==T){
    # Y(A1,M0)
    p.am <- with(dOther.val,expit(0.95+0.28*as.numeric(Sex=='1')+0.014*Age))# true parameters (datagenererende) M for A=0
    dOther.val[,M:=factor(p.am)]
    p.Y.A1M0 <- with(dOther.val,expit(-7.33+0.075*as.numeric(Sex=='1')+0.067*Age+0.092*as.numeric(M=='1')))# true parameters (datagenererende) Y for A1
    # Y(A1,M1)
    p.Y.A1M1<-dOther.val[,mean(Y)]
    
    results<-c(true.Y.A1M0=mean(p.Y.A1M0),true.Y.A1M1=p.Y.A1M1,True.ATE=mean(p.Y.A1M0)-p.Y.A1M1)
    return(results)
    
  } else if (model=='glm'){
      #GLM      
      #M(A0)
      #create mediator-model for chest pain patients 
      amBryst<-glm(formula.m,family = binomial(),data=dBryst)
      # predict chance of receiving an ambulance among other symptoms
      p.am<- predictRisk(amBryst,newdata=dOther.val)
      
      #Y(A1)
      #create outcome-model for other symptoms 
      mort.Other<-glm(formula.y,family = binomial(),data=dOther)
      
      # Estimated Y(A1,M1)
      p.Y.A1M1<-predictRisk(mort.Other,newdata=dOther.val)
      
      # Estimated Y(A1,M0)
      # evaluate change in outcome in hypothetical setting
      
      loopresult.YA1M0 <- foreach(b=1:100,.combine="rbind") %dopar%{
        # new variable ambulance represents if patient has received
        # ambulance in the hypothetical situation
        hyp.other <- copy(dOther.val)
        hyp.other[,M:=factor(rbinom(.N,1,p.am))]
        #predict the risk of dying when patients without chest pain received
        #an ambulance as often as patients with chest pain
        predictRisk(mort.Other,newdata=hyp.other)
      }
      result.YA1M0 <- apply(loopresult.YA1M0,2,mean)
      
      results.glm<-c(glm.Y.A1M0=mean(result.YA1M0),
                     glm.Y.A1M1=mean(p.Y.A1M1),
                     glm.ATE=mean(result.YA1M0)-mean(p.Y.A1M1))
      return(results.glm)
      } else if (model=='superlearner'){
        #SL      
        #M(A0)
        #create mediator-model for chest pain patients 
        amBryst<-SuperLearner(Y=dBryst[,as.numeric(M==1)],
                              X=dBryst[,.(Sex,Age)],
                              family = binomial(),
                              SL.library = sl.lib)
        
        # predict chance of receiving an ambulance among other symptoms
        p.am<-predict(amBryst, newdata = dOther.val[,.(Sex,Age)], onlySL = T)
        
        #Y(A1)
        #create outcome-model for other symptoms
        mortOther<-SuperLearner(Y=dOther[,as.numeric(Y==1)],
                                X=dOther[,.(Sex,Age,as.numeric(M==1))],
                                family = binomial(),
                                SL.library = sl.lib)
        
        # Estimated Y(A1,M1)
        p.Y.A1M1<-predict(mortOther, newdata = dOther.val[,.(Sex,Age,as.numeric(M==1))],onlySL = T)$pred
        
        # Estimated Y(A1,M0)
        # evaluate change in outcome in hypothetical setting
        loopresult.YA1M0 <- foreach(b=1:100,.combine="rbind") %dopar%{
          # new variable ambulance represents if patient has received
          # ambulance in the hypothetical situation
          hyp.other <- copy(dOther.val)
          hyp.other[,M:=factor(rbinom(.N,1,p.am$pred[,1]))]
          #hyp.other[,A:=0]
          #predict the risk of dying when patients without chest pain received
          #an ambulance as often as patients with chest pain
          res<-predict(mortOther,newdata=hyp.other[,.(Sex,Age,as.numeric(M==1))],onlySL = T)$pred[,1]
        }
    
    result.YA1M0 <- apply(loopresult.YA1M0,2,mean)
    
    results.SL<-c(SL.Y.A1M0=mean(result.YA1M0),
                  SL.Y.A1M1=mean(p.Y.A1M1),
                  SL.ATE=mean(result.YA1M0)-mean(p.Y.A1M1))
    
    model.SL<-c(mediator.model=amBryst$coef,
                outcome.model=mortOther$coef)
    
    result<-list(result=results.SL,mediator.model=amBryst$coef,outcome.model=mortOther$coef)
    
    return(result)
  } else 
    print('NA')
  
}
  


# simulate data for simulation

set.seed(1); d<- sim(m1,5000) 
setDT(d)
d[A=='1',M:=M1]
d[A=='0',M:=M0]
d[A=='1',Y:=Y1]
d[A=='0',Y:=Y0]

d<-d[,.(id=1:.N,
     A=factor(A),
     M=factor(M),
     Age=Age,
     Sex=factor(Sex),
     Y=Y)]

str(d)

# define library for superlearner
sl.lib<-c('SL.glmnet','SL.randomForest','SL.gam','SL.mean')

# specify formula for mediator glm
formula.m<-(M~Sex+Age)
# specify formula for outcome glm
formula.y<-(Y~Sex+Age+M)


# model can take values c('glm','superlearner')
# true.parameter can be T/F
y<-ambulance(data=d,
          A=A,
          n=5000,
          n.train=3000,
          model='superlearner',
          formula.m=formula.m,
          formula.y=formula.y,
          sl.lib=sl.lib,
          true.parameters=F);y

