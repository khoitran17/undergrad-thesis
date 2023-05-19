library(tidyverse)
library(pretrends)
library(readxl)
library(logr)

anyBeta <- read_excel("Pretrend_Data.xlsx", sheet = "AnyBeta")
anyBeta <- anyBeta$Coefficient
anySigma <- read_excel("Pretrend_Data.xlsx", sheet = "AnySigma")
anySigma <- subset(anySigma, select = -c(1))
anyTVec <- c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

pubBeta <- read_excel("Pretrend_Data.xlsx", sheet = "PubBeta")
pubBeta <- pubBeta$Coefficient
pubSigma <- read_excel("Pretrend_Data.xlsx", sheet = "PubSigma")
pubSigma <- subset(pubSigma, select = -c(1))
pubTVec <- c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

privBeta <- read_excel("Pretrend_Data.xlsx", sheet = "PrivBeta")
privBeta <- privBeta$Coefficient
privSigma <- read_excel("Pretrend_Data.xlsx", sheet = "PrivSigma")
privSigma <- subset(privSigma, select = -c(1))
privTVec <-  c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

referencePeriod <- 0

anyDf <- data.frame(t = anyTVec, beta = anyBeta)
pubDf <- data.frame(t = pubTVec, beta = pubBeta)
privDf <- data.frame(t = privTVec, beta = privBeta)

anyslope80 <- slope_for_power(sigma = anySigma,
                           targetPower = 0.8,
                           tVec = anyTVec,
                           referencePeriod = referencePeriod)

anyslope80

anypretrendsResults <- pretrends(betahat = anyBeta, 
                              sigma = anySigma, 
                              tVec = anyTVec, 
                              referencePeriod = referencePeriod,
                              deltatrue = anyslope80 * (anyTVec - referencePeriod))

anypretrendsResults$event_plot
anypretrendsResults$df_power
anypretrendsResults$event_plot
anypretrendsResults$event_plot_pretest




pubslope80 <- slope_for_power(sigma = pubSigma,
                              targetPower = 0.8,
                              tVec = pubTVec,
                              referencePeriod = referencePeriod)

pubslope80

pubpretrendsResults <- pretrends(betahat = pubBeta, 
                                 sigma = pubSigma, 
                                 tVec = pubTVec, 
                                 referencePeriod = referencePeriod,
                                 deltatrue = pubslope80 * (pubTVec - referencePeriod))

pubpretrendsResults$event_plot
pubpretrendsResults$df_power
pubpretrendsResults$event_plot
pubpretrendsResults$event_plot_pretest


privslope80 <- slope_for_power(sigma = privSigma,
                              targetPower = 0.8,
                              tVec = privTVec,
                              referencePeriod = referencePeriod)

privslope80

privpretrendsResults <- pretrends(betahat = privBeta, 
                                 sigma = privSigma, 
                                 tVec = privTVec, 
                                 referencePeriod = referencePeriod,
                                 deltatrue = privslope80 * (privTVec - referencePeriod))

privpretrendsResults$event_plot
privpretrendsResults$df_power
privpretrendsResults$event_plot
pubpretrendsResults$event_plot_pretest


