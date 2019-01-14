model{
  mulambda[1]<-mean(log(lambda[c(NT)]))
  mulambda[2]<-mean(log(lambda[c(QLD)]))
  mulambda[3]<-mean(log(lambda[c(WA)]))
  ##model fit
  #bpvalue <- mean(test)      # Bayesian p-value  
  #test <- step(fit.rep - fit)   # Test whether new data set more extreme
  #fit <- sum(res.y[])           # test statistic for data   
  #fit.rep <- sum(res.y.rep[])   # test statistic for new predicted data   
  #predictions<-sum(y.rep[])
  #for (j in 1:942){
    #calculate goodness of fit statistic for logistic regression model using data; squared pearson residuals
    #res.y[j] <- ((y0[j] - predicted[j]) / (sqrt(predicted[j]*(1-predicted[j]))))^2
    
    # calculate goodness of fit statistic for logistic regression model using new predicted data
    #res.y.rep[j] <- ((y.rep[j] - predicted[j]) / (sqrt(predicted[j]*(1-predicted[j]))))^2
    
    #y.rep[j] ~ dbern(predicted[j])
    #predicted[j]<-Ez[j,1]
  #}
  
  
  ##dynamic model into future; assumes nearly 100% persistence if invasion occurs 
  for (j in 1:942){
    for (t in 2:11){
      z[j,t]~dbern(Ez[j,t]) 
      Ez[j,t]<-gamma[j,t]*(1-z[j,t-1])+z[j,t-1]
      ##Ez is p(presence)
      ##gamma is P(invasion), z is latent presence of toads
    }
  }
  for (j in 1:942){  ##j is island
    y0[j]~dbern(Ez[j,1])
    z[j,1]<-y0[j]
    Ez[j,1]<-gamma[j,1]
  }
  ###model for invasion/persistence probability
  for (j in 1:942){ ##j is island
    for (t in 1:11){
      gamma[j,t]<-(1-neggamma[j,t])
      neggamma[j,t]<-exp((-lambda[j]*mu.time[j,t]))
      itime[j,t]~dpois(mu.time[j,t])
      mu.time[j,t]~dunif(0.01,150)
    }
  }
  ##estimating rate
  for (j in 1:942){ ##j is island
    log(lambda[j])<- beta0+ 
      #beta1*scdmain[j]+
      beta2*scarea[j]+
      beta3*scelevMax[j]+
      #beta4*scRugMean[j]+
      beta5*sclights[j]+
      beta6*scdriver[j]+
      #beta7*scCyc[j]+
      beta8*scdriver[j]*scCyc[j]
  }
  
  beta0 ~ dnorm(0.0,0.001)
  beta1 ~ dnorm(0.0,0.001)
  beta2 ~ dnorm(0.0,0.001)
  beta3 ~ dnorm(0.0,0.001)
  beta4 ~ dnorm(0.0,0.001)
  beta5 ~ dnorm(0.0,0.001)
  beta6 ~ dnorm(0.0,0.001)
  
  #beta7 ~ dnorm(0.0,0.5)
  beta8 ~ dnorm(0.0,0.05)
  
  for (j in 1:942){ 
    scRugMean[j] ~ dnorm(mu.RugMean,tau.RugMean)
  }
  for (j in 1:942){ 
    scelevMax[j] ~ dnorm(mu.elevMax,tau.elevMax)
  } 
  
  for (j in 1:942){ 
    sclights[j] ~ dnorm(mu.lights,tau.lights)
  }  
  
  
  ##hyperparameters for imputing missing values
  mu.RugMean ~ dnorm(0,0.001)
  tau.RugMean ~ dgamma(0.001,0.001)
  mu.elevMax ~ dnorm(0,0.001)
  tau.elevMax ~ dgamma(0.001,0.001)
  mu.lights ~ dnorm(0,0.001)
  tau.lights ~ dgamma(0.001,0.001)
}