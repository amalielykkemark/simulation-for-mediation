
out<-list()
for (i in 1:3) {
  set.seed(i+1234)
  out[[i]] <- eif.function.mediation(data=sim.data(n=50))
}


library(lava)
run <- function(...,n=5000) {
  d <- sim.data(n=n)
  f <- eif.function.mediation(data=d)
  f
}

run()
x<-sim(run,10000)

#true parameters
true.dat<-sim.data(n=2e7)

true.psi1 <- mean(true.dat[A==0,Y1])
true.psi2 <- mean(true.dat[A==0,Y0])
true.psi<-true.psi1-true.psi2


summary(x,true=c(true.psi1,true.psi2,true.psi))

# plot estimate + true etsimates
plot.new()
density(x[,1:3])
abline(v=c(true.psi,true.psi1,true.psi2),col=2)



