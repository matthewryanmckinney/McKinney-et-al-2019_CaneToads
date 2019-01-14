##McKinney 2019 Thesis Chapter 3 script for running analysis
##data and model can be found at
#scdmain is distance to mainland for each island, scaled to mean=0 and var=1
#sclights is stable night lights intensity for each island, scaled to mean=0 and var=1
#scarea is area for each island, scaled to mean=0 and var=1
#scelevmax is max elevation for each island, scaled to mean=0 and var=1
#scrugmean is mean terrain ruggedness index for each island, scaled to mean=0 and var=1
#y0 is presence or pseudoabsence of toads for each island at t=1
#itime is estimated years since nearest mainland invasion for  each island
#scdriver is distance to major perennial river mout for each island, scaled to mean=0 and var=1
#scCyc is cyclone frequency and intensity index for each island, scaled to mean=0 and var=1
#WA is indicator variable for Western Australia
#QLD is indicator variable for Queensland
#NT is indicator variable for Northern Territory

library(snow)
library(rjags)
library(runjags)
library(DHARMa)
library(R2jags)
## The data list
forjags<-list("scdmain"=scdmain,"sclights"=sclights,"scarea"=scarea,"scelevMax"=scelevMax,
            "scRugMean"=scRugMean,"y0"=y0,"itime"=itime,"scdriver"=scdriver,"scCyc"=scCyc,
            "WA"=WA,"QLD"=QLD,"NT"=NT)

## parameters-to-keep list 
parms<-c("mulambda", "beta0","beta2","beta3","beta5","beta6","beta8",
         "Ez","lambda")
parms<-c("mulambda", "beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta8",
         "Ez","lambda")
         

##########################################
### Initiating values 


### Bare minimum list of initial values

j.inits<-list(list(beta0=beta0.init,beta1=beta1.init,beta2=beta2.init,beta3=beta3.init,beta4=beta4.init,beta5=beta5.init,
                  beta6=beta6.init,beta8=beta8.init,mu.time=(itime+1)),
              list(beta0=beta0.init2,beta1=beta1.init2,beta2=beta2.init2,beta3=beta3.init2,beta4=beta4.init2,beta5=beta5.init2,
                  beta6=beta6.init2,beta8=beta8.init2,mu.time=(itime+1)),
              list(beta0=beta0.init3,beta1=beta1.init3,beta2=beta2.init3,beta3=beta3.init3,beta4=beta4.init3,beta5=beta5.init3,
                  beta6=beta6.init3,beta8=beta8.init3,mu.time=(itime+1)))
              
memory.limit(size=8000000)
#cjags<-jags.model(file="C:/Users/uqmmckin/Documents/Code/ToadIsles_16102017.R",inits=j.inits,n.chains=2,n.adapt=1000,data=forjags)
#update(cjags,n.iter=1000)
#cjagsout<-coda.samples(cjags,n.iter=1000,thin=10,variable.names=parms)

cjagsout<-run.jags(model="C:/Users/uqmmckin/Documents/Code/ToadIsles_06072018_correm.R",
                   monitor=parms,data=forjags,n.chains=3,inits=j.inits,burnin=50000,adapt=1000,
                   sample=5000,thin=10,jags.refresh=60,method="rjags",modules = "glm on",summarise = TRUE)
save.image("C:/Users/uqmmckin/Documents/PhD/Pubs/Cane Toads/MCMC/toads_correm.RData")

out_fortest<-jags(model.file="C:/Users/uqmmckin/Documents/Code/ToadIsles_06072018_correm.R",
               parameters.to.save=c("y.rep","predicted"),data=forjags,n.chains=3,inits=j.inits,n.burnin=50000,
               n.iter=100000,n.thin=10,refresh=60,jags.module = "glm",DIC=F)
save.image("C:/Users/uqmmckin/Documents/PhD/Pubs/Cane Toads/MCMC/toads_correm.RData")

##DHarma goodness of fit
simulations = out_fortest$BUGSoutput$sims.list$y.rep
dim(simulations)
pred = apply(out_fortest$BUGSoutput$sims.list$predicted, 2, median)
sim = createDHARMa(simulatedResponse = t(simulations), observedResponse = y0, fittedPredictedResponse = pred, integerResponse = T)
plotSimulatedResiduals(sim)

save.image("C:/Users/uqmmckin/Documents/PhD/Pubs/Cane Toads/MCMC/toads_correm.RData")

#cjags<-jags.model(file="C:/Users/uqmmckin/Documents/Code/ToadIsles_revNoKrig_Normtime_dlnormD2_y0DefNTP0307.R",inits=j.inits,n.chains=3,n.adapt=1000,data=forjags)
#update(cjags,n.iter=20000)
#cjagsout<-coda.samples(cjags,n.iter=10000,thin=2,variable.names=parms)

#summary(cjagsout)
#plot(cjagsout)

results<-summary(cjagsout)
