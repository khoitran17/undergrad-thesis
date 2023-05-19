library(tidyverse)
library(pretrends)
library(readxl)
library(logr)

dyAnyBeta <- read_excel("Pretrend_Data.xlsx", sheet = "DyAnyBeta")
dyAnyBeta <- dyAnyBeta$Coefficient
dyAnySigma <- read_excel("Pretrend_Data.xlsx", sheet = "DyAnySigma")
dyAnySigma <- subset(dyAnySigma, select = -c(1))
dyAnyTVec <- c(-2, -1, 1, 2, 3, 4)

dyPubBeta <- read_excel("Pretrend_Data.xlsx", sheet = "DyPubBeta")
dyPubBeta <- dyPubBeta$Coefficient
dyPubSigma <- read_excel("Pretrend_Data.xlsx", sheet = "DyPubSigma")
dyPubSigma <- subset(dyPubSigma, select = -c(1))
dyPubTVec <- c(-2, -1, 1, 2, 3, 4)

dyPrivBeta <- read_excel("Pretrend_Data.xlsx", sheet = "DyPrivBeta")
dyPrivBeta <- dyPrivBeta$Coefficient
dyPrivSigma <- read_excel("Pretrend_Data.xlsx", sheet = "DyPrivSigma")
dyPrivSigma <- subset(dyPrivSigma, select = -c(1))
dyPrivTVec <- c(-2, -1, 1, 2, 3, 4)

referencePeriod <- 0

anyDf <- data.frame(t = dyAnyTVec, beta = dyAnyBeta)
pubDf <- data.frame(t = dyPubTVec, beta = dyPubBeta)
privDf <- data.frame(t = dyPrivTVec, beta = dyPrivBeta)

anyslope80 <- slope_for_power(sigma = dyAnySigma,
                              targetPower = 0.8,
                              tVec = dyAnyTVec,
                              referencePeriod = referencePeriod)

anyslope80

anypretrendsResults <- pretrends(betahat = dyAnyBeta, 
                                 sigma = dyAnySigma, 
                                 tVec = dyAnyTVec, 
                                 referencePeriod = referencePeriod,
                                 deltatrue = anyslope80 * (dyAnyTVec - referencePeriod))

anypretrendsResults$event_plot
anypretrendsResults$df_power
anypretrendsResults$event_plot
anypretrendsResults$event_plot_pretest

pubslope80 <- slope_for_power(sigma = dyPubSigma,
                              targetPower = 0.8,
                              tVec = dyPubTVec,
                              referencePeriod = referencePeriod)

pubslope80

pubpretrendsResults <- pretrends(betahat = dyPubBeta, 
                                 sigma = dyPubSigma, 
                                 tVec = dyPubTVec, 
                                 referencePeriod = referencePeriod,
                                 deltatrue = pubslope80 * (dyPubTVec - referencePeriod))

pubpretrendsResults$event_plot
pubpretrendsResults$df_power
pubpretrendsResults$event_plot
pubpretrendsResults$event_plot_pretest

privslope80 <- slope_for_power(sigma = dyPrivSigma,
                              targetPower = 0.8,
                              tVec = dyPrivTVec,
                              referencePeriod = referencePeriod)

privslope80

privpretrendsResults <- pretrends(betahat = dyPrivBeta, 
                                 sigma = dyPrivSigma, 
                                 tVec = dyPrivTVec, 
                                 referencePeriod = referencePeriod,
                                 deltatrue = privslope80 * (dyPrivTVec - referencePeriod))

privpretrendsResults$event_plot
privpretrendsResults$df_power
privpretrendsResults$event_plot
privpretrendsResults$event_plot_pretest



