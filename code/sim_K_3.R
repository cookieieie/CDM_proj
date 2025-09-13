library(mirt)
r=matrix(0.5,3,3)
r
diag(r) <- 1
r
Theta <- mvtnorm::rmvnorm(2000,sigma=r)
head(Theta)

a <- matrix(c(
  .8,NA,NA,
  .8,NA,NA,
  1,NA,NA,
  .8,NA,NA,
  .8,NA,NA,
  1,NA,NA,
  1,NA,1.4,
  1,NA,.4,
  1,NA,2.4,
  NA,NA,.4,
  NA,NA,.4,
  NA,NA,.4,
  NA,.8,1,
  NA,.4,1,
  NA,.9,1,
  NA,1.4,NA,
  NA,.4,NA,
  NA,1.4,NA),ncol=3,byrow=TRUE)
dim(a)
d <- matrix(rnorm(nrow(a)))
d
itemtype <- rep('2PL',nrow(a))

simmirt <- simdata(a=a, d=d, itemtype=itemtype, Theta=Theta)

model <- '
F1 = 1-9
F2 = 7-15
F3 = 13-18
COV=F1*F2*F3'
mod <- mirt(simmirt, model)
coef(mod)

library(GDINA)
Q=(!is.na(a))*1
est=GDINA(simmirt,Q)

x1=personparm(est,"mp") # mastery probability for each attribute
head(x1)

x2=fscores(mod) # theta for each dimension from mirt
head(x2)

plot(qnorm(x1[,1]),x2[,1])
cor(qnorm(x1[,1]),x2[,1])

