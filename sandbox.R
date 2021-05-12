library(lava)
library(Publish)
source('~/GitHub/Project2/simulation-for-mediation/sim_data.R')
source('~/GitHub/Project2/simulation-for-mediation/eif_function_mediation.R')

# A=1 chest pain, A=0 No chest pain
# Z=1 emergency ambulance
# Y=1 dead at 30-day follow-up


# positivity problem
publish(f <- glm(Z~A+Age+Sex+EMS,data=d,family="binomial"))
d[, gammahat.a1:=predict(f, newdata=copy(d)[, A:=1], type="response")]
d[,max(gammahat.a1)] 


# psi1 #
# the probability of dying among patients without chest pain had their 
# probability of receiving an emergency ambulance been the same as for patients with chest pain 

# psi2 #
# the probability of dying among patients without chest pain had their 
# probability of receiving an emergency ambulance been as observed

# psi #
# target parameter, the difference between the two parameters above (psi1 - psi2)


# pihat #
#the probability of a patient presenting with chest pain

# gammahat.a1 #
#the probability of receiving an emergency ambulance had all presented with chest pain

# gammahat.a0 #
#the probability of receiving an emergency ambulance had all presented without chest pain


# sim.data is shown with the default parameter settings
eif.function.mediation(data=sim.data(n=5000,
                                     prob_sex = list(prob=0.65),
                                     prob_ems = list(prob=0.65),
                                     prob_age = list(Intercept=71.7,Sex=-5.8),
                                     logOR_A = list(Intercept=2.5,Sex=0.44, Age=-0.024),
                                     logOR_Z = list(Intercept=-1.5,Sex=0.07, Age=0.0004,EMS=1.85,A=2.83),
                                     logOR_Y = list(Intercept=-8, Sex=0.13, Age=0.075,A=-1.36, Z=0.27)))



#true parameters
true.dat<-sim.data(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.07,Age=0.0004,EMS=1.85,A=0.83))

true.psi1 <- mean(true.dat[A==0,Y1])
true.psi2 <- mean(true.dat[A==0,Y0])
true.psi<-true.psi1-true.psi2

run <- function(...,n=5000) {
  d <- sim.data(n=n,logOR_Z=list(Intercept=-1.5,Sex=0.07,Age=0.0004,EMS=1.85,A=0.83))
  f <- eif.function.mediation(data=d)
  f
}
run()

set.seed(9)
x<-sim(run,1000)

summary(x,true=c(true.psi1,true.psi2,true.psi))

# plot estimate + true etsimates
plot.new()
density(x[,1:3])
abline(v=c(true.psi,true.psi1,true.psi2),col=2)



