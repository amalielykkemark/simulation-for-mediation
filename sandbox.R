library(lava)
library(Publish)
library(ggplot2)
library(ggforce)
library(patchwork)

source('~/GitHub/Project2/simulation-for-mediation/sim_data.R')
source('~/GitHub/Project2/simulation-for-mediation/eif_function_mediation.R')
source('~/GitHub/Project2/simulation-for-mediation/eif_function_fixed.R')
source('~/GitHub/Project2/simulation-for-mediation/additional_functions.R')

theme_set(theme_light(base_size = 18))

options(scipen = 999, digits = 3)
# A=1 chest pain, A=0 No chest pain
# Z=1 emergency ambulance
# Y=1 dead at 30-day follow-up

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

###########################################################################################
# 1 illustrate problem and increase n ----
true.psi<-true.function(n=1e7)

# illustarte problem with increasing n
sim1<-sim.machine(n.run = 10000,n=1000)
sim5<-sim.machine(n.run = 10000,n=5000)
sim10<-sim.machine(n.run = 10000,n=10000)
sim20<-sim.machine(n.run = 10000,n=20000)

# create table of results
tab_problem_by_n<-as.data.frame(rbind(table.machine(data=sim1),
                                      table.machine(data=sim5),
                                      table.machine(data=sim10),
                                      table.machine(data=sim20)))
                        
setDT(tab_problem_by_n)
tab_problem_by_n[,names:=rep(c('psi1','psi2','psi'),4)]
tab_problem_by_n[,n:=c(rep(1000,3),rep(5000,3),rep(10000,3),rep(20000,3))]
tab_problem_by_n<-copy(tab_problem_by_n[,.(names,n,bias,sd.mean,se.eif,cov)])
setkey(tab_problem_by_n,names,n)

write.table(format(tab_problem_by_n,digits=3),file='H:/Early warning signs/Projekt 2/Tabeller/tab_problem_by_n.csv', sep=';')

# illustrate problem with eic1 values ----
eic.5000<-eif.function.mediation(data=sim.data(n=5000),
                                 dataset = T)
ggplot(eic.5000,aes(x=eic1))+
  geom_histogram(binwidth=0.5,fill='darkgreen')+
  scale_x_continuous(breaks=seq(-1,13,1))+
  facet_zoom(xlim = c(11, 13), 
             ylim = c(0, 20), 
             horizontal = F)

eic.5000[eic1>10,.N,keyby=.(A,Z,Y)]

###########################################################################################
# 2. solve problem of psi1 by intervening on P(Z|A) ----
true.psi<-true.function(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7))
true.psi2<-true.function(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2))
true.psi1<-true.function(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=1))
true.psi05<-true.function(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0.5))
true.psi025<-true.function(n=2e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0.25))
true.psi0<-true.function(n=1e8,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0))


# Simulate data when changing effect of A in P(Z|A)
sim.z<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7))
sim.z2<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2))
sim.z1<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=1))
sim.z05<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0.5))
sim.z025<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0.25))
sim.z0<-sim.machine(n.run = 20000,n=5000,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0))

# create table of results
tab_z<-as.data.frame(rbind(table.machine(data=sim.z, true.psi = true.psi),
                           table.machine(data=sim.z2,true.psi = true.psi2),
                           table.machine(data=sim.z1,true.psi = true.psi1),
                           table.machine(data=sim.z05,true.psi = true.psi05),
                           table.machine(data=sim.z05,true.psi = true.psi025),
                           table.machine(data=sim.z0,true.psi = true.psi0)))


tab_z<-as.data.frame(rbind(table.machine(data=sim.z025,true.psi = true.psi025),
                           table.machine(data=sim.z0,true.psi = true.psi0)))

setDT(tab_z)
tab_z[,names:=rep(c('psi1','psi2','psi'),6)]
tab_z[,n:=c(rep(2.7,3),rep(2,3),rep(1,3),rep(0.5,3),rep(0.25,3),rep(0,3))]
tab_z<-copy(tab_z[,.(names,n,bias,sd.mean,se.eif,cov)])
setkey(tab_z,names,n)

write.table(format(tab_z,digits=3),file='H:/Early warning signs/Projekt 2/Tabeller/tab_z.csv', sep=';')

###########################################################################################
# 3. Show how eic1 values change with changing A in P(Z|A)
  res<-eif.function.mediation(data = sim.data(n=1e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7)), dataset = T)
  res[,pz:='2.7']
  res2<-eif.function.mediation(data = sim.data(n=1e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2)), dataset = T)
  res2[,pz:='2']
  res1<-eif.function.mediation(data = sim.data(n=1e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=1)), dataset = T)
  res1[,pz:='1']
  res05<-eif.function.mediation(data = sim.data(n=1e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0.5)), dataset = T)
  res05[,pz:='0.5']
  res0<-eif.function.mediation(data = sim.data(n=1e7,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=0)), dataset = T)
  res0[,pz:='0']

d<-rbind(res,res1,res05);d

ggplot(data=d,aes(x=eic1, fill=pz))+
  geom_histogram(binwidth=0.05)+
  scale_fill_manual(values=c('darkgreen','skyblue','brown'))+
  scale_x_continuous(breaks=seq(-1,12,1))+
  facet_zoom(xlim = c(5, 12), 
             ylim = c(0, 700), 
             horizontal = F,
             zoom.size = 1)+
  labs(title='Distribution of influence function values for\nEIC1 when changing coefficients for A',y='Count', x='Influence function values',
       fill='Coefficients of A in P(Z|A)')+
  theme(strip.background = element_rect(fill='beige', colour='grey'),
        legend.position = 'bottom')

ggsave(file='H:/Early warning signs/Projekt 2/Figurer/eic1.png')

ggplot(data=d,aes(x=eic, fill=pz))+
  geom_histogram(binwidth=0.05)+
  scale_fill_manual(values=c('darkgreen','skyblue','brown'))+
  scale_x_continuous(breaks=seq(-4,8,1))+
  facet_zoom(ylim = c(0, 1000), 
             horizontal = T,
             zoom.size = 2)+
  labs(title='Distribution of influence function values for\nEIC when changing coefficients for A',y='Count', x='Influence function values',
       fill='Coefficients of A in P(Z|A)')+
  theme(strip.background = element_rect(fill='beige', colour='grey'),
        legend.position = 'bottom')

ggsave(file='H:/Early warning signs/Projekt 2/Figurer/eic.png')

###########################################################################################
# test the simple version of teh estimation - do we improve the coverage
sim.50<-sim.machine(n.run = 1000,
                    n=5000,
                    prob.mediator=0.5,
                    eif.mediation = F)

sim.70<-sim.machine(n.run = 1000,
                    n=5000,
                    prob.mediator=0.7,
                    eif.mediation = F)

sim.90<-sim.machine(n.run = 1000,
                    n=5000,
                   prob.mediator=0.9,
                   eif.mediation = F)

# simulate true values for fixed probaility of 
true.psi.50<-true.function(n=2e7,fixed.prob.ZA1 = 0.5)
true.psi.70<-true.function(n=2e7,fixed.prob.ZA1 = 0.7)
true.psi.90<-true.function(n=2e7,fixed.prob.ZA1 = 0.9)

# tabulate results
tab.fixed<-as.data.frame(rbind(table.machine(data=sim.50, true.psi = true.psi.50),
                           table.machine(data=sim.70, true.psi = true.psi.70),
                           table.machine(data=sim.90, true.psi = true.psi.90)))
setDT(tab.fixed)
tab.fixed[,psi:=rep(c('psi1','psi2','psi'),3)]
tab.fixed[,names:=c(rep('P(Z|A=1)=0.5',3),
                    rep('P(Z|A=1)=0.7',3),
                    rep('P(Z|A=1)=0.9',3))]
setkey(tab.fixed,psi,names)
tab.fixed

write.table(format(tab.fixed,digits=3),file='H:/Early warning signs/Projekt 2/Tabeller/tabfixed.csv', sep=';')

