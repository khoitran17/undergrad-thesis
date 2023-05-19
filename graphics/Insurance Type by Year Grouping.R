library(ipumsr)
library(tidyverse)
cps_ddi_file <- "cps_00004.xml"
cps_data_file <- "cps_00004.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

df <- as.data.frame(cps_data)

cpsdf <- df[, c("YEAR", "AGE", "HCOVANY", "HCOVPRIV", "HCOVPUB")]
cpsdf <- subset(cpsdf, YEAR <= 2008)

cps9095 <- subset(cpsdf,  YEAR >= 1990 & YEAR <= 1995)
cps9903 <- subset(cpsdf,  YEAR >= 1999 & YEAR <= 2003)
cps0408 <- subset(cpsdf,  YEAR >= 2004 & YEAR <= 2008)

#5a

tbl9095any <- table(cps9095[, c("AGE", "HCOVANY")])
tbl9903any <- table(cps9903[, c("AGE", "HCOVANY")])
tbl0408any <- table(cps0408[, c("AGE", "HCOVANY")])

tbl9095any <- tbl9095any / rowSums(tbl9095any) * 100
tbl9903any <- tbl9903any / rowSums(tbl9903any) * 100
tbl0408any <- tbl0408any / rowSums(tbl0408any) * 100

tbl9095anydf <- as.data.frame(tbl9095any)
tbl9903anydf <- as.data.frame(tbl9903any)
tbl0408anydf <- as.data.frame(tbl0408any)

hasany9095 <- tbl9095anydf %>% filter(tbl9095anydf$HCOVANY == 2)
hasany9903 <- tbl9903anydf %>% filter(tbl9903anydf$HCOVANY == 2)
hasany0408 <- tbl0408anydf %>% filter(tbl0408anydf$HCOVANY == 2)

hasany9095 <- hasany9095[, c("AGE", "Freq")]
hasany9903 <- hasany9903[, c("AGE", "Freq")]
hasany0408 <- hasany0408[, c("AGE", "Freq")]

hasany9095 <- hasany9095[17:37, ]
hasany9903 <- hasany9903[17:37, ]
hasany0408 <- hasany0408[17:37, ]

hasany9095 <- hasany9095 %>% rename(Age = AGE, Percent9095 = Freq)
hasany9903 <- hasany9903 %>% rename(Age = AGE, Percent9903 = Freq)
hasany0408 <- hasany0408 %>% rename(Age = AGE, Percent0408 = Freq)

combinedany <- merge(hasany9095, hasany9903, by = "Age")
combinedany <- merge(combinedany, hasany0408, by = "Age")

plot5 <-ggplot(combinedany, aes(x = Age)) + 
        geom_line(aes(y = Percent9095, color = "1990-1995"), group = 1) +
        geom_line(aes(y = Percent9903, color = "1999-2003"), linetype = "twodash", group = 1) + 
        geom_line(aes(y = Percent0408, color = "2004-2008"), linetype = "dashed", group = 1) +
        ylab("Percent Covered") + 
        scale_y_continuous(limits = c(50, 100), breaks=c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)) + 
        scale_color_manual("Years", breaks = c("1990-1995", "1999-2003", "2004-2008"), values = c("red", "blue", "darkgreen"))

plot5 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.title = element_text(size = 16),
                           axis.text = element_text(size = 14),
                           legend.text = element_text(size = 14),
                           legend.title = element_text(size = 16))

#5b

tbl9095pub <- table(cps9095[, c("AGE", "HCOVPUB")])
tbl9903pub <- table(cps9903[, c("AGE", "HCOVPUB")])
tbl0408pub <- table(cps0408[, c("AGE", "HCOVPUB")])

tbl9095pub <- tbl9095pub / rowSums(tbl9095pub) * 100
tbl9903pub <- tbl9903pub / rowSums(tbl9903pub) * 100
tbl0408pub <- tbl0408pub / rowSums(tbl0408pub) * 100

tbl9095pubdf <- as.data.frame(tbl9095pub)
tbl9903pubdf <- as.data.frame(tbl9903pub)
tbl0408pubdf <- as.data.frame(tbl0408pub)

haspub9095 <- tbl9095pubdf %>% filter(tbl9095pubdf$HCOVPUB == 2)
haspub9903 <- tbl9903pubdf %>% filter(tbl9903pubdf$HCOVPUB == 2)
haspub0408 <- tbl0408pubdf %>% filter(tbl0408pubdf$HCOVPUB == 2)

haspub9095 <- haspub9095[, c("AGE", "Freq")]
haspub9903 <- haspub9903[, c("AGE", "Freq")]
haspub0408 <- haspub0408[, c("AGE", "Freq")]

haspub9095 <- haspub9095[17:37, ]
haspub9903 <- haspub9903[17:37, ]
haspub0408 <- haspub0408[17:37, ]

haspub9095 <- haspub9095 %>% rename(Age = AGE, Percent9095 = Freq)
haspub9903 <- haspub9903 %>% rename(Age = AGE, Percent9903 = Freq)
haspub0408 <- haspub0408 %>% rename(Age = AGE, Percent0408 = Freq)

combinedpub <- merge(haspub9095, haspub9903, by = "Age")
combinedpub <- merge(combinedpub, haspub0408, by = "Age")

plot5b <- ggplot(combinedpub, aes(x = Age)) + 
  geom_line(aes(y = Percent9095, color = "1990-1995"), group = 1) +
  geom_line(aes(y = Percent9903, color = "1999-2003"), linetype = "twodash", group = 1) + 
  geom_line(aes(y = Percent0408, color = "2004-2008"), linetype = "dashed", group = 1) +
  ylab("Percent Covered") + 
  scale_y_continuous(limits = c(0, 40), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40)) + 
  scale_color_manual("Years", breaks = c("1990-1995", "1999-2003", "2004-2008"), values = c("red", "blue", "darkgreen"))

plot5b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.title = element_text(size = 16),
                           axis.text = element_text(size = 14),
                           legend.text = element_text(size = 14),
                           legend.title = element_text(size = 16))



#5c

tbl9095priv <- table(cps9095[, c("AGE", "HCOVPRIV")])
tbl9903priv <- table(cps9903[, c("AGE", "HCOVPRIV")])
tbl0408priv <- table(cps0408[, c("AGE", "HCOVPRIV")])

tbl9095priv <- tbl9095priv / rowSums(tbl9095priv) * 100
tbl9903priv <- tbl9903priv / rowSums(tbl9903priv) * 100
tbl0408priv <- tbl0408priv / rowSums(tbl0408priv) * 100

tbl9095privdf <- as.data.frame(tbl9095priv)
tbl9903privdf <- as.data.frame(tbl9903priv)
tbl0408privdf <- as.data.frame(tbl0408priv)

haspriv9095 <- tbl9095privdf %>% filter(tbl9095privdf$HCOVPRIV == 2)
haspriv9903 <- tbl9903privdf %>% filter(tbl9903privdf$HCOVPRIV == 2)
haspriv0408 <- tbl0408privdf %>% filter(tbl0408privdf$HCOVPRIV == 2)

haspriv9095 <- haspriv9095[, c("AGE", "Freq")]
haspriv9903 <- haspriv9903[, c("AGE", "Freq")]
haspriv0408 <- haspriv0408[, c("AGE", "Freq")]

haspriv9095 <- haspriv9095[17:37, ]
haspriv9903 <- haspriv9903[17:37, ]
haspriv0408 <- haspriv0408[17:37, ]

haspriv9095 <- haspriv9095 %>% rename(Age = AGE, Percent9095 = Freq)
haspriv9903 <- haspriv9903 %>% rename(Age = AGE, Percent9903 = Freq)
haspriv0408 <- haspriv0408 %>% rename(Age = AGE, Percent0408 = Freq)

combinedpriv <- merge(haspriv9095, haspriv9903, by = "Age")
combinedpriv <- merge(combinedpriv, haspriv0408, by = "Age")

plot5c <- ggplot(combinedpriv, aes(x = Age)) + 
  geom_line(aes(y = Percent9095, color = "1990-1995"), group = 1) +
  geom_line(aes(y = Percent9903, color = "1999-2003"), linetype = "twodash", group = 1) + 
  geom_line(aes(y = Percent0408, color = "2004-2008"), linetype = "dashed", group = 1) +
  ylab("Percent Covered") + 
  scale_y_continuous(limits = c(40, 80), breaks=c(40, 45, 50, 55, 60, 65, 70, 75, 80)) + 
  scale_color_manual("Years", breaks = c("1990-1995", "1999-2003", "2004-2008"), values = c("red", "blue", "darkgreen"))

plot5c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                            axis.title = element_text(size = 16),
                            axis.text = element_text(size = 14),
                            legend.text = element_text(size = 14),
                            legend.title = element_text(size = 16))

