library(data.table)

#true parameters function
true.function<-function(n=2e6,logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7),
                        fixed.prob.ZA1=NA){
  if (is.na(fixed.prob.ZA1)){
    true.dat<-sim.data(n=n,logOR_Z =logOR_Z)  
  }
  
  if (!is.na(fixed.prob.ZA1)){
    true.dat<-sim.data(n=n,fixed.prob.ZA1 = fixed.prob.ZA1)  
  }
  
  true<-c('true.psi1'=mean(true.dat[A==0,Y1]),
          'true.psi2'=mean(true.dat[A==0,Y0]),
          'true.psi'=mean(true.dat[A==0,Y1])-mean(true.dat[A==0,Y0]))
  return(true)
}

# function where you can change number of simulations (n.run), 
#number of observations (n) and 
#changes distribution of Z (logOR_Z=list())
sim.machine<-function(n.run=n.run,n=n,
                      logOR_Z=list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7),
                      eif.mediation=T,
                      prob.mediator=prob.mediator){
  run <- function(...) {
    d <- sim.data(n=n,logOR_Z = logOR_Z)
    
    if (eif.mediation==T){
      f <- eif.function.mediation(data=d)
    }
    if (eif.mediation==F){
      f <- eif.fixed(data=d,prob.mediator=prob.mediator)
    }
    f
  }
  sim(run,n.run)
}


#estimate coverage fundtion
table.machine<-function(data=data, true.psi=true.psi){
  
  bias<-c(psi1=mean(data[,1])-true.psi[1],
          psi2=mean(data[,2])-true.psi[2],
          psi=mean(data[,3])-true.psi[3])
  
  sd.mean<-c(psi1=sd(data[,1]),
             psi2=sd(data[,2]),
             psi=sd(data[,3]))
  
  se.eif<-c(psi1=mean(data[,4]),
            psi2=mean(data[,5]),
            psi=mean(data[,6]))
  
  cov<-c(psi1=mean(unlist(data[,1])-1.96*unlist(data[,4])<=true.psi[1] & unlist(data[,1])+1.96*unlist(data[,4])>=true.psi[1]),
         psi2=mean(unlist(data[,2])-1.96*unlist(data[,5])<=true.psi[2] & unlist(data[,2])+1.96*unlist(data[,5])>=true.psi[2]),
         psi=mean(unlist(data[,3])-1.96*unlist(data[,6])<=true.psi[3] & unlist(data[,3])+1.96*unlist(data[,6])>=true.psi[3]))
  
  out<-cbind(bias,sd.mean,se.eif,cov)
  out}
