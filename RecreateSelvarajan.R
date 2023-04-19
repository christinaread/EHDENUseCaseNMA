##Frequentist (as they did in article)
library(readxl)
ResultsSelvarajan <- read_excel("Recreating Selvarajan.xlsx")
View(ResultsSelvarajan)

install.packages("netmeta")
library(netmeta)

?netmeta
m.netmetaselvarajan <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = treat1,
                     treat2 = treat2,
                     studlab = Author,
                     data = ResultsSelvarajan,
                     sm = "RR",
                     fixed = TRUE,
                     random = FALSE,
                     reference.group = "Tocilizumab",
                     details.chkmultiarm = TRUE,
                     sep.trts = " vs ",
                     tol.multiarm = 1,
                     backtransf = TRUE)

summary(m.netmetaselvarajan)

plot(m.netmetaselvarajan, 
       reference.group = "SC",
       labels = m.netmetaselvarajan$trts,
       xlim = c(-1.5, 0.5))

netheat(m.netmetaselvarajan)

netsplit(m.netmetaselvarajan) %>% forest()

funnel(m.netmetaselvarajan510only, 
       order = c("5 days", "10 days", "tocilizumab"), 
       linreg = TRUE)








#################################################################################################
#Try it Bayesian since Frequentist is not working properly
#################################################################################################
ResultsBayesianSelvarajan <- read_excel("Bayesian Recreating Selvarajan.xlsx")
View(ResultsBayesianSelvarajan)

library(gemtc)
library(rjags)

networkSelvarajanB <- mtc.network(data.ab  = ResultsBayesianSelvarajan)
summary(networkSelvarajanB)
plot(networkSelvarajanB, 
     use.description = TRUE) # Use full treatment names

# We give our compiled model the name `model`.
modelSelvarajanB <- mtc.model(networkSelvarajanB,
                   link = "logit",
                   linearModel = "random",
                   n.chain = 3)

mcmcSelvarajanB <- mtc.run(modelSelvarajanB, n.adapt = 10000, n.iter = 100000, thin = 10)
plot(mcmcSelvarajanB)

gelman.plot(mcmcSelvarajanB)

gelman.diag(mcmcSelvarajanB)$mpsrf

gemtc::forest(relative.effect(mcmcSelvarajanB, t1 = "SC"),
              xlim = c(-3, 3))


gemtc::forest(relative.effect(mcmcSelvarajanB, t1 = "Tocilizumab"), 
              xlim = c(-2, 0.5))

nodesplit <- mtc.nodesplit(networkSelvarajan, 
                           link = "logit",
                           n.adapt = 10000, 
                           n.iter = 100000, 
                           thin = 10)
summary(nodesplit)
plot(summary(nodesplit)) 
