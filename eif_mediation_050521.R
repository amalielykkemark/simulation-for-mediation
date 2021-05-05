#-----#
#install.packages('pracma')
# libraries ----- 
library(SuperLearner)
library(data.table)
library(lava)
library(Hmisc)
library(ggplot2)
library(riskRegression)
library(prodlim)
library(Publish)
library(pracma)
library(patchwork)

options(scipen=999)
#----------------
#-- compute true  

n<-1e7
set.seed(123)
Sex <- rbinom(n, 1,prob=0.65)
EMS <- rbinom(n, 1, prob=0.65)

Age <- rnorm(n, 71.7-5.8*Sex)
A <- rbinom(n, 1, plogis(2.5+0.44*Sex-0.024*Age))
Z.A1 <- rbinom(n, 1, plogis(-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.83*1))
Z.A0 <- rbinom(n, 1, plogis(-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.83*0))
Y1 <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z.A1))
Y0 <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z.A0))

d <- data.table(id=1:n, Y0,Y1, A)
d[,.N,A]
d[,.N,Y0]
d[,.N,Y1]
d[,.N,EMS]

(true.psi1 <- mean(d[A==0,Y1]))
(true.psi2 <- mean(d[A==0,Y0]))
true.psi<-true.psi1-true.psi2;true.psi


#----------------
#-- observed data  

#-- sample size
n <- 5000

#-- estimators
psi2.list <- list()
psi1.list <- list()

#-- estimator for risk difference
psi.diff.list <- list()

#-- se estimators
se1.list <- list()
se2.list <- list()

#-- se of risk difference
se.diff.list <- list()


#-- assess possible positivity violations
max.gammahata1.list <- list()
max.pihat.list <- list()
#--------------------
#- repeat simulations

for (jj in 1:1000) {
  
  set.seed(jj+1234)# set random seed

  Sex <- rbinom(n, 1,prob=0.65)
  EMS <- rbinom(n, 1, prob=0.65)
  Age <- rnorm(n, 71.7-5.8*Sex)
  A <- rbinom(n, 1, plogis(2.5+0.44*Sex-0.024*Age))
  Z <- rbinom(n, 1, plogis(-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.83*A))
  Y <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z))
  
  dt <- data.table(id=1:n,
                  EMS=EMS,
                  Age=Age,
                  Sex=as.numeric(Sex),
                  A=as.numeric(A),
                  Z=as.numeric(Z),
                  Y=as.numeric(Y))
  
  dt[,.N,Y]
  #-- initial estimation; 
  
  Qfit <- glm(Y~Z+A+Sex+Age, family=binomial, data=dt)
  gammafit <- glm(Z~A+Sex+Age+EMS, family=binomial, data=dt)
  pifit <- glm(A~Sex+Age, family=binomial, data=dt)
  pibar <- dt[,mean(A==0)]
  
  dt[, pihat:=predict(pifit, newdata=dt, type="response")]
  
  dt[, gammahat.a1:=predict(gammafit, newdata=copy(dt)[, A:=1], type="response")]
  dt[, gammahat.a0:=predict(gammafit, newdata=copy(dt)[, A:=0], type="response")]
  
  dt.full <- data.table(rbind(copy(dt)[, Z:=1],
                              copy(dt)[, Z:=0]),
                        Z.obs=c(dt[, Z], dt[, Z]))
  
  dt.full[, summary(pihat)]
  dt.full[, summary(gammahat.a1)]
  dt.full[, summary(gammahat.a0)]
  
  max.gammahata1.list[[jj]] <-dt.full[, summary(gammahat.a1)[6]]
  
  max.pihat.list[[jj]] <- dt.full[, summary(pihat)[6]]
  
  
  dt.full[, Qhat:=predict(Qfit, newdata=dt.full, type="response")]
  
  dt.full[, Qhat.a1:=predict(Qfit, newdata=copy(dt.full)[, A:=1], type="response")]
  dt.full[, Qhat.a0:=predict(Qfit, newdata=copy(dt.full)[, A:=0], type="response")]
  
  dt.full[, psi.1:=sum(Qhat.a0*(gammahat.a1*Z+(1-gammahat.a1)*(1-Z))), by="id"]
  dt.full[, psi.0:=sum(Qhat.a0*(gammahat.a0*Z+(1-gammahat.a0)*(1-Z))), by="id"]
  
  psi1.list[[jj]] <- psihat.1 <- dt.full[Z==Z.obs, (1/pibar)*mean((A==0)*psi.1)]
  
  psi2.list[[jj]] <- psihat.2 <- dt.full[Z==Z.obs, (1/pibar)*mean((A==0)*psi.0)]
  
  psi.diff.list[[jj]] <- psi.hat <- psihat.1 - psihat.2
  
  eic1 <- dt.full[Z==Z.obs, ((A==0)/pibar*((Z*gammahat.a1+(1-Z)*(1-gammahat.a1))/ 
                                             (Z*gammahat.a0+(1-Z)*(1-gammahat.a0)))*
                               (Y - Qhat.a0) +
                               (A==1)/pihat*(1-pihat)/pibar*(Qhat.a0 - psi.1) + 
                               (A==0)/pibar*(psi.1 - psihat.1))]
  
  eic2 <- dt.full[Z==Z.obs, ((A==0)/pibar*(Y - Qhat.a0) +
                               (A==0)/pibar*(Qhat.a0 - psi.0) + 
                               (A==0)/pibar*(psi.0 - psihat.2))]
  
  se1.list[[jj]] <- sqrt(mean(eic1^2)/nrow(dt))
  
  se2.list[[jj]] <- sqrt(mean(eic2^2)/nrow(dt))
  
  se.diff.list[[jj]] <-  sqrt(mean((eic1-eic2)^2)/nrow(dt))
  
}

par(mfrow=c(1,3))
hist(unlist(psi1.list)); abline(v=true.psi1, col="red"); abline(v=mean(unlist(psi1.list)), col="blue")
hist(unlist(psi2.list)); abline(v=true.psi2, col="red"); abline(v=mean(unlist(psi2.list)), col="blue")
hist(unlist(psi.diff.list)); abline(v=true.psi1-true.psi2, col="red"); abline(v=mean(unlist(psi.diff.list)), col="blue")

theme_set(theme_classic())
dat.psi<-as.data.frame(cbind(psi1=unlist(psi1.list),psi2=unlist(psi2.list),psi=unlist(psi.diff.list)))

plot.psi1<-ggplot(dat.psi)+
  geom_histogram(aes(psi1),fill='slategrey',bins = 10)+
  geom_vline(xintercept=as.numeric(true.psi1), color='orange')+
  geom_vline(xintercept=mean(unlist(psi1.list)), color='blue')

plot.psi2<-ggplot(dat.psi)+
  geom_histogram(aes(psi2),fill='slategrey', bins=10)+
  geom_vline(xintercept=as.numeric(true.psi2), color='orange')+
  geom_vline(xintercept=mean(unlist(psi2.list)), color='blue')

plot.psi<-ggplot(dat.psi)+
  geom_histogram(aes(psi),fill='slategrey', bins=10)+
  geom_vline(xintercept=as.numeric(true.psi), color='orange')+
  geom_vline(xintercept=mean(unlist(psi.diff.list)), color='blue')

plot.psi1+plot.psi2+plot.psi

mean(unlist(psi1.list));true.psi1
mean(unlist(psi2.list));true.psi2
mean(unlist(psi.diff.list));true.psi

min(unlist(max.gammahata1.list));mean(unlist(max.gammahata1.list));max(unlist(max.gammahata1.list))
min(unlist(max.pihat.list));mean(unlist(max.pihat.list));max(unlist(max.pihat.list))


(cov1 <- mean(unlist(psi1.list)-1.96*unlist(se1.list)<=true.psi1 &
                unlist(psi1.list)+1.96*unlist(se1.list)>=true.psi1))

(cov2 <- mean(unlist(psi2.list)-1.96*unlist(se2.list)<=true.psi2 &
                unlist(psi2.list)+1.96*unlist(se2.list)>=true.psi2))

(cov.diff <- mean(unlist(psi.diff.list)-1.96*unlist(se.diff.list)<=(true.psi1-true.psi2) &
                    unlist(psi.diff.list)+1.96*unlist(se.diff.list)>=(true.psi1-true.psi2)))

(oracle.cov.diff <- mean(unlist(psi.diff.list)-1.96*sd(unlist(psi.diff.list))<=(true.psi1-true.psi2) &
                           unlist(psi.diff.list)+1.96*sd(unlist(psi.diff.list))>=(true.psi1-true.psi2)))
cov.diff;cov1;cov2

sd(unlist(psi1.list)); mean(unlist(se1.list))
sd(unlist(psi2.list)); mean(unlist(se2.list))

sd(unlist(psi.diff.list)); mean(unlist(se.diff.list))

