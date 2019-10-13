library(relevent)
## Not run:
#Generate some simple sample data based on fixed effects
roweff<-rnorm(10)                                     #Build rate matrix
roweff<-roweff-roweff[1]                   #Adjust for later convenience
coleff<-rnorm(10)
coleff<-coleff-coleff[1]
lambda<-exp(outer(roweff,coleff,"+"))
diag(lambda)<-0
ratesum<-sum(lambda)
esnd<-as.vector(row(lambda))                  #List of senders/receivers
erec<-as.vector(col(lambda))
time<-0
edgelist<-vector()
while(time<15){                   # Observe the system for 15 time units
  drawsr<-sample(1:100,1,prob=as.vector(lambda))        #Draw from model
  time<-time+rexp(1,ratesum)
  if(time<=15)                                             #Censor at 15
    edgelist<-rbind(edgelist,c(time,esnd[drawsr],erec[drawsr]))
  else
    edgelist<-rbind(edgelist,c(15,NA,NA))
}

#Fit the model, ordinal BPM
effects<-c("FESnd","FERec")
fit <-rem.dyad(edgelist,10,effects=effects,hessian=TRUE)

saveRDS(fit, "rem_dyad_64.RData")
fit_64 <- readRDS("rem_dyad_64.RData")
fit_32 <- readRDS("rem_dyad_32.RData")

expect_equivalent(fit_64, fit_32)

Error: `fit_64` not equivalent to `fit_32`.
Component “par”: Mean relative difference: 0.8041537
Component “value”: Mean relative difference: 0.06462584
Component “counts”: Mean relative difference: 0.03937008
Component “hessian”: Mean relative difference: 0.5847839
Component “coef”: Mean relative difference: 0.8041537
Component “m”: Mean relative difference: 0.09036145
Component “residual.deviance”: Mean relative difference: 0.3786668
Component “model.deviance”: Mean relative difference: 1.215159
Component “residuals”: Numeric: lengths (6474, 5889) differ
