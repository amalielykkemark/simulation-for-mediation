
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

################################### M1 simple data set #######################################
## OBS find ud af hvorfor det ikke passer???
#Create empty object
m1<-lvm()
# Varibles and distributions
distribution(m1,~Y0) <-binomial.lvm(p=0.91)
distribution(m1,~Y1) <-binomial.lvm(p=0.972)
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
regression(m1) <- A~ -0.59*W1+0.23*W2-0.14*W3_1-0.12*W3_2-0.29*W4 + 0.013*W5

regression(m1) <- M~-2.49*A+0.18*W1-0.21*W2-0.02*W3_1-0.29*W3_2+0.16*W4+0.007*W5
regression(m1) <- Y0~ -0.20*W1 + 0.44*W2 - 0.09*W3_1-0.07*W3_2 + 0.15*W4 + 0.075*W5+ 0.12*M
regression(m1) <- Y1~ 1.16 -0.20*W1 + 0.44*W2 - 0.09*W3_1-0.07*W3_2 + 0.15*W4 + 0.075*W5+ 0.12*M

#Check regression
glm(data = simulate(m1, nsim = 1e5), formula = A~W1+W2+W3+W4+W5)
glm(data = simulate(m1, nsim = 1e4), formula = Y0~A+W1+W2+W3+W4+W5+M)
glm(data = simulate(m1, nsim = 1e4), formula = Y1~A+W1+W2+W3+W4+W5+M)

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
data3<-data_mc[,c("W1","W2","W3","W4","A","M","Y")]

