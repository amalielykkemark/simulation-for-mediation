

sim.data <- function(n=n) {
requireNamespace("data.table")
    Sex <- rbinom(n, 1,prob=0.65)
  EMS <- rbinom(n, 1, prob=0.65)
  
  Age <- rnorm(n, 71.7-5.8*Sex)
  A <- rbinom(n, 1, plogis(2.5+0.44*Sex-0.024*Age))
  Z.A1 <- rbinom(n, 1, plogis(-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.83*1))
  Z.A0 <- rbinom(n, 1, plogis(-1.5+0.07*Sex+0.0004*Age+1.85*EMS+2.83*0))
  Z <- Z.A1*A+Z.A0*(1-A)
  Y1 <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z.A1))
  Y0 <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z.A0))
  Y <- rbinom(n, 1, plogis(-8-1.36*A+0.13*Sex+0.075*Age+0.27*Z))
  
  d <- data.table::data.table(id=1:n,Age,Sex,EMS,A, Z, Y, Y0,Y1)
  return(d)
}

