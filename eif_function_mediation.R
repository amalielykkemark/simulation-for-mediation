library(data.table)



eif.function.mediation<-function(data=data, dataset=F){
  requireNamespace("data.table")
  
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
  max.gammahata0.list <- list()
  max.pihat.list <- list()
  
  dt<-copy(data)
  data.table::setDT(dt)
  
  Qfit <- glm(Y~Z+A+Sex+Age, family=binomial, data=dt)
  gammafit <- glm(Z~A+Sex+Age, family=binomial, data=dt)
  pifit <- glm(A~Sex+Age, family=binomial, data=dt)
  pibar <- dt[,mean(A==0)]
  
  dt[, pihat:=predict(pifit, newdata=dt, type="response")]
  dt[, gammahat.a1:=predict(gammafit, newdata=copy(dt)[, A:=1], type="response")]
  dt[, gammahat.a0:=predict(gammafit, newdata=copy(dt)[, A:=0], type="response")]
  
  dt.full <- data.table(rbind(copy(dt)[, Z:=1],
                              copy(dt)[, Z:=0]),
                        Z.obs=c(dt[, Z], dt[, Z]))
  
  dt.full[, Qhat:=predict(Qfit, newdata=dt.full, type="response")]
  
  dt.full[, Qhat.a1:=predict(Qfit, newdata=copy(dt.full)[, A:=1], type="response")]
  dt.full[, Qhat.a0:=predict(Qfit, newdata=copy(dt.full)[, A:=0], type="response")]
  
  dt.full[, psi.1:=sum(Qhat.a0*(gammahat.a1*Z+(1-gammahat.a1)*(1-Z))), by="id"]
  dt.full[, psi.0:=sum(Qhat.a0*(gammahat.a0*Z+(1-gammahat.a0)*(1-Z))), by="id"]
  
  psi1.list <- psihat.1 <- dt.full[Z==Z.obs, (1/pibar)*mean((A==0)*psi.1)]
  
  psi2.list <- psihat.2 <- dt.full[Z==Z.obs, (1/pibar)*mean((A==0)*psi.0)]
  
  psi.diff.list <- psi.hat <- psihat.1 - psihat.2
  
  dt.full[Z==Z.obs,eic1:=((A==0)/pibar*((Z*gammahat.a1+(1-Z)*(1-gammahat.a1))/ 
                                             (Z*gammahat.a0+(1-Z)*(1-gammahat.a0)))*
                               (Y - Qhat.a0) +
                               (A==1)/pihat*(1-pihat)/pibar*(Qhat.a0 - psi.1) + 
                               (A==0)/pibar*(psi.1 - psihat.1))]
  eic1<-dt.full[Z==Z.obs,eic1]
  

  dt.full[Z==Z.obs,eic2:=((A==0)/pibar*(Y - Qhat.a0) +
                               (A==0)/pibar*(Qhat.a0 - psi.0) + 
                               (A==0)/pibar*(psi.0 - psihat.2))]

  eic2<-dt.full[Z==Z.obs,eic2]
  
  dt.full[Z==Z.obs,eic:=eic1-eic2]
  
  se1.list <- sqrt(mean(eic1^2)/nrow(dt))
  
  se2.list <- sqrt(mean(eic2^2)/nrow(dt))
  
  se.diff.list <-  sqrt(mean((eic1-eic2)^2)/nrow(dt))

  if (dataset==T){
    out<-dt.full[Z==Z.obs]
  }
  
  if (dataset==F){
  out<-c(psi1=psi1.list,
            psi2=psi2.list,
            psi=psi.diff.list,
            se1=se1.list,
            se2=se2.list,
            se.diff=se.diff.list,
         max.gammahat.a1=dt.full[, summary(gammahat.a1)[6]],
         max.gammahat.a0=dt.full[, summary(gammahat.a0)[6]],
         max.pihat=dt.full[, summary(pihat)[6]])}
  
  return(out)
  
}

#eif.function.mediation(data=data)
#eif.function.mediation(data=data,dataset = T)



