library(tidyverse)
library(pretrends)
library(readxl)

anyBeta <- read_excel("Pretrend Data.xlsx", sheet = "AnyBeta")
anyBeta <- anyBeta$Coefficient
anySigma <- read_excel("Pretrend Data.xlsx", sheet = "AnySigma")
anySigma <- subset(anySigma, select = -c(1))
anyTVec <- c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

pubBeta <- read_excel("Pretrend Data.xlsx", sheet = "PubBeta")
pubBeta <- pubBeta$Coefficient
pubSigma <- read_excel("Pretrend Data.xlsx", sheet = "PubSigma")
pubSigma <- subset(pubSigma, select = -c(1))
pubTVec <- c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

privBeta <- read_excel("Pretrend Data.xlsx", sheet = "PrivBeta")
privBeta <- privBeta$Coefficient
privSigma <- read_excel("Pretrend Data.xlsx", sheet = "PrivSigma")
privSigma <- subset(privSigma, select = -c(1))
privTVec <-  c(-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

referencePeriod <- 0

anyDf <- data.frame(t = anyTVec, beta = anyBeta)
pubDf <- data.frame(t = pubTVec, beta = pubBeta)
privDf <- data.frame(t = privTVec, beta = privBeta)

slope50 <- slope_for_power(sigma = anySigma,
                           targetPower = 0.5,
                           tVec = anyTVec,
                           referencePeriod = referencePeriod)

pretrendsResults <- pretrends(betahat = anyBeta, 
                              sigma = anySigma, 
                              tVec = anyTVec, 
                              referencePeriod = referencePeriod,
                              deltatrue = slope50 * (anyTVec - referencePeriod))

#Error in dyn.load(file, DLLpath = DLLpath, ...) :unable to load shared object '/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/gmm/libs/gmm.so':dlopen(/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/gmm/libs/gmm.so, 0x0006): Library not loaded: /opt/R/arm64/gfortran/lib/libgomp.1.dylib Referenced from: <47242657-5A5D-3982-936B-398527D642B4> /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/gmm/libs/gmm.so Reason: tried: '/opt/R/arm64/gfortran/lib/libgomp.1.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/R/arm64/gfortran/lib/libgomp.1.dylib' (no such file), '/opt/R/arm64/gfortran/lib/libgomp.1.dylib' (no such file), '/usr/local/lib/libgomp.1.dylib' (no such file), '/usr/lib/libgomp.1.dylib' (no such file, not in dyld cache)