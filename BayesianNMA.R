library(readxl)
ResultsBayesianFormat <- read_excel("ResultsBayesianFormat.xlsx")
View(ResultsBayesianFormat)

install.packages("gemtc")
library(gemtc)
install.packages("rjags")
library(rjags)
?mtc.model
?mtc.network


network <- mtc.network(data.ab  = ResultsBayesianFormat)
summary(network)
plot(network, 
     use.description = TRUE) # Use full treatment names

anohe <- mtc.anohe(network)
plot(summary(anohe))

# We give our compiled model the name `model`.
model <- mtc.model(network,
                   link = "logit",
                   linearModel = "random",
                   n.chain = 3)

mcmc1 <- mtc.run(model, n.adapt = 50, n.iter = 1000, thin = 10)
mcmc2 <- mtc.run(model, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmc1)
plot(mcmc2)

#mcm2 has better convergence

gelman.plot(mcmc1)
gelman.plot(mcmc2)

gelman.diag(mcmc1)$mpsrf
gelman.diag(mcmc2)$mpsrf


gemtc::forest(relative.effect(mcmc2, t1 = "PlaceboOrStandardCare"),
       xlim = c(-1.5, 1.5))


gemtc::forest(relative.effect(mcmc2, t1 = "Tocilizumab"), 
       xlim = c(-1.5, 0.5))



#################################################################################################
#check results when removing trials with 0 results
#################################################################################################
ResultsBayesianFormatMinus0trials <- read_excel("ResultsBayesianFormatMinus0trials.xlsx")
View(ResultsBayesianFormatMinus0trials)

networkminus0trials <- mtc.network(data.ab  = ResultsBayesianFormatMinus0trials)
summary(networkminus0trials)
plot(networkminus0trials, 
     use.description = TRUE) # Use full treatment names

# We give our compiled model the name `model`.
modelminus0trials <- mtc.model(networkminus0trials,
                   link = "logit",
                   linearModel = "random",
                   n.chain = 3)

mcmcminus0trials <- mtc.run(modelminus0trials, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcminus0trials)

#convergence is ok

gelman.plot(mcmcminus0trials)

gelman.diag(mcmcminus0trials)$mpsrf
#psrf below 1.05

gemtc::forest(relative.effect(mcmcminus0trials, t1 = "PlaceboOrStandardCare"),
              xlim = c(-1.5, 1.5))


gemtc::forest(relative.effect(mcmcminus0trials, t1 = "Tocilizumab"), 
              xlim = c(-1.5, 0.5))                   


#################################################################################################
#check results when just using BMJ trials
#################################################################################################
ResultsBayesianFormatJustBMJ <- read_excel("ResultsBayesianFormatJustBMJ.xlsx")
View(ResultsBayesianFormatJustBMJ)

networkJustBMJ <- mtc.network(data.ab  = ResultsBayesianFormatJustBMJ)
summary(networkJustBMJ)
plot(networkJustBMJ, 
     use.description = TRUE) # Use full treatment names

anoheJustBMJ <- mtc.anohe(networkJustBMJ)
plot(summary(anoheJustBMJ))

# We give our compiled model the name `model`.
modeljustBMJ <- mtc.model(networkJustBMJ,
                               link = "logit",
                               linearModel = "random",
                               n.chain = 3)

mcmcJustBMJ <- mtc.run(modeljustBMJ, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcJustBMJ)

#convergence is ok

gelman.plot(mcmcJustBMJ)

gelman.diag(mcmcJustBMJ)$mpsrf
#psrf below 1.05

gemtc::forest(relative.effect(mcmcJustBMJ, t1 = "PlaceboOrStandardCare"),
              xlim = c(-1.5, 1.5))


gemtc::forest(relative.effect(mcmcJustBMJ, t1 = "Tocilizumab"), 
              xlim = c(-1.5, 0.5))      


#################################################################################################
#check results when just using Selvarajan trials and separating remdesivir into 5 or 10 day
#################################################################################################


ResultsBayesianSelvarajan <- read_excel("Bayesian Recreating Selvarajan.xlsx")
View(ResultsBayesianSelvarajan)

library(gemtc)
library(rjags)

networkSelvarajanB <- mtc.network(data.ab  = ResultsBayesianSelvarajan)
summary(networkSelvarajanB)
plot(networkSelvarajanB, 
     use.description = TRUE) # Use full treatment names

anoheSelvarajanB <- mtc.anohe(networkSelvarajanB)
plot(summary(anoheSelvarajanB))

# We give our compiled model the name `model`.
modelSelvarajanB <- mtc.model(networkSelvarajanB,
                              link = "logit",
                              linearModel = "random",
                              n.chain = 3)

mcmcSelvarajanB <- mtc.run(modelSelvarajanB, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcSelvarajanB)

gelman.plot(mcmcSelvarajanB)

gelman.diag(mcmcSelvarajanB)$mpsrf

gemtc::forest(relative.effect(mcmcSelvarajanB, t1 = "PlaceboOrStandardCare"),
              xlim = c(-3, 3))


gemtc::forest(relative.effect(mcmcSelvarajanB, t1 = "Tocilizumab"), 
              xlim = c(-2, 0.5))

nodesplit <- mtc.nodesplit(networkSelvarajanB, 
                           link = "logit",
                           n.adapt = 10000, 
                           n.iter = 100000, 
                           thin = 10)


summary(nodesplit)
plot(summary(nodesplit)) 
#why does this only show one node? not useful like this

#################################################################################################
#Corticosteroid Analysis
#################################################################################################
ResultsBayesianFormatCorticosteroids <- read_excel("ResultsBayesianFormatCorticosteroids.xlsx")
View(ResultsBayesianFormatCorticosteroids)

networkCorticosteroids <- mtc.network(data.ab  = ResultsBayesianFormatCorticosteroids)
summary(networkCorticosteroids)
plot(networkCorticosteroids, 
     use.description = TRUE) # Use full treatment names

anoheCorticosteroids <- mtc.anohe(networkCorticosteroids)
plot(summary(anoheCorticosteroids))

# We give our compiled model the name `model`.
modelCorticosteroids <- mtc.model(networkCorticosteroids,
                               link = "logit",
                               linearModel = "random",
                               n.chain = 3)

mcmcmCorticosteroids <- mtc.run(modelCorticosteroids, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcmCorticosteroids)

#convergence is ok

gelman.plot(mcmcmCorticosteroids)

gelman.diag(mcmcmCorticosteroids)$mpsrf
#psrf below 1.05

gemtc::forest(relative.effect(mcmcmCorticosteroids, t1 = "PlaceboOrStandardCare"),
              xlim = c(-1.5, 1.5))


gemtc::forest(relative.effect(mcmcmCorticosteroids, t1 = "Tocilizumab"), 
              xlim = c(-1.5, 0.5))    

#################################################################################################
#Corticosteroid Analysis only BMJ articles
#################################################################################################
ResultsBayesianFormatBMJCorticosteroids <- read_excel("ResultsBayesianFormatBMJCorticosteroids.xlsx")
View(ResultsBayesianFormatBMJCorticosteroids)

networkBMJCorticosteroids <- mtc.network(data.ab  = ResultsBayesianFormatBMJCorticosteroids)
summary(networkBMJCorticosteroids)
plot(networkBMJCorticosteroids, 
     use.description = TRUE) # Use full treatment names

anoheBMJCorticosteroids <- mtc.anohe(networkBMJCorticosteroids)
plot(summary(anoheBMJCorticosteroids))

# We give our compiled model the name `model`.
modelBMJCorticosteroids <- mtc.model(networkBMJCorticosteroids,
                                  link = "logit",
                                  linearModel = "random",
                                  n.chain = 3)

mcmcmBMJCorticosteroids <- mtc.run(modelBMJCorticosteroids, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcmCorticosteroids)

#convergence is ok

gelman.plot(mcmcmBMJCorticosteroids)

gelman.diag(mcmcmBMJCorticosteroids)$mpsrf
#psrf below 1.05

gemtc::forest(relative.effect(mcmcmBMJCorticosteroids, t1 = "PlaceboOrStandardCare"),
              xlim = c(-1.5, 1.5))


gemtc::forest(relative.effect(mcmcmBMJCorticosteroids, t1 = "Tocilizumab"), 
              xlim = c(-1.5, 0.5)) 
