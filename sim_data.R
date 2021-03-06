

sim.data <- function(n=n,
                     prob_sex = list(prob=0.65),
                     prob_age = list(Intercept=71.7,Sex=-5.8),
                     logOR_A = list(Intercept=2.5,Sex=0.44, Age=-0.024),
                     logOR_Z = list(Intercept=-1.5,Sex=0.12, Age=0.0078,A=2.7),
                     logOR_Y = list(Intercept=-8, Sex=0.13, Age=0.075,A=-1.36, Z=0.27),
                     fixed.prob.ZA1=NA) {
requireNamespace("data.table")
  Sex <- rbinom(n, 1,prob=prob_sex$prob)
  Age <- rnorm(n, prob_age$Intercept+prob_age$Sex*Sex)
  A <- rbinom(n, 1, plogis(logOR_A$Intercept+logOR_A$Sex*Sex+logOR_A$Age*Age))
  if (!is.na(fixed.prob.ZA1)){
    Z.A1<-rbinom(n,1,prob=fixed.prob.ZA1)    
  }
  if (is.na(fixed.prob.ZA1)){
    Z.A1 <- rbinom(n, 1, plogis(logOR_Z$Intercept+logOR_Z$Sex*Sex+logOR_Z$Age*Age+logOR_Z$A))
  }
  Z.A0 <- rbinom(n, 1, plogis(logOR_Z$Intercept+logOR_Z$Sex*Sex+logOR_Z$Age*Age))
  Z <- Z.A1*A+Z.A0*(1-A)
  Y1 <- rbinom(n, 1, plogis(logOR_Y$Intercept+logOR_Y$Sex*Sex+logOR_Y$Age*Age+logOR_Y$A*A+logOR_Y$Z*Z.A1))
  Y0 <- rbinom(n, 1, plogis(logOR_Y$Intercept+logOR_Y$Sex*Sex+logOR_Y$Age*Age+logOR_Y$A*A+logOR_Y$Z*Z.A0))
  Y <-  rbinom(n, 1, plogis(logOR_Y$Intercept+logOR_Y$Sex*Sex+logOR_Y$Age*Age+logOR_Y$A*A+logOR_Y$Z*Z))
  
  d <- data.table::data.table(id=1:n,Age,Sex,A, Z, Y, Y0,Y1)
  return(d)
}

data<-sim.data(n=1000)
#data<-sim.data(n=1000,fixed.prob.ZA1 = 0.9)

#summary(data)
