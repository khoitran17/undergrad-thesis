library(ipumsr)
library(tidyverse)
cps_ddi_file <- "cps_00008.xml"
cps_data_file <- "cps_00008.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

df <- as.data.frame(cps_data)

cpsdf <- df[, c("YEAR", "RELATE", "AGE", "MARST", "PHIOWN", "GRPDEPLY", "HCOVPUB", "HCOVPRIV", "PHINSUR", "COVERPI")]
cpsdf <- subset(cpsdf, AGE <= 35)
cpsdf <- subset(cpsdf, AGE >= 16)
cpsdf <- subset(cpsdf, YEAR <= 2009)

dependent <- cpsdf %>% filter(cpsdf$MARST == 6)
dependent <- dependent[, c("AGE", "GRPDEPLY")]
dependenttbl <- table(dependent)
dependenttbl <- dependenttbl / rowSums(dependenttbl) * 100
dependenttbldf <- as.data.frame(dependenttbl)
dependenttbldf <- dependenttbldf %>% filter(dependenttbldf$GRPDEPLY == 2)

private <- cpsdf[, c("AGE", "PHIOWN", "GRPDEPLY", "MARST")]
private <- private %>% mutate(isCovered = case_when(PHIOWN == 2 ~ 2,
                                                    MARST == 1 & GRPDEPLY == 2 ~ 2,
                                                    MARST == 2 & GRPDEPLY == 2 ~ 2))
private <- private[, c("AGE", "isCovered")]
private[is.na(private)] <- 1
privatetbl <- table(private)
privatetbl <- privatetbl/ rowSums(privatetbl) * 100
privatetbldf <- as.data.frame(privatetbl)
privatetbldf <- privatetbldf %>% filter(privatetbldf$isCovered == 2)

public <- cpsdf[, c("AGE", "HCOVPUB")]
publictbl <- table(public)
publictbl <- publictbl/ rowSums(publictbl) * 100
publictbldf <- as.data.frame(publictbl)
publictbldf <- publictbldf %>% filter(publictbldf$HCOVPUB == 2)

combined <- merge(dependenttbldf, privatetbldf, by = "AGE")
combined <- merge(combined, publictbldf, by = "AGE")
combined <- combined %>% rename(Age = AGE, ParentPlan = Freq.x, OwnPlan = Freq.y, PublicPlan = Freq)
combined <- combined[, c("Age", "ParentPlan", "OwnPlan", "PublicPlan")]

combinedparent <- combined[, c("Age", "ParentPlan")]
combinedparent <- combinedparent %>% rename(Freq = ParentPlan)
combinedparent$Group <- "Parent"

combinedprivate <- combined[, c("Age", "OwnPlan")]
combinedprivate <- combinedprivate %>% rename(Freq = OwnPlan)
combinedprivate$Group <- "Own"

combinedpublic <- combined[, c("Age", "PublicPlan")]
combinedpublic<- combinedpublic %>% rename(Freq = PublicPlan)
combinedpublic$Group <- "Public"

combinededited <- rbind(combinedparent, combinedprivate, combinedpublic)
combinededited$Freq <- as.double(combinededited$Freq)
combinededited$Age <- as.numeric(as.character(combinededited$Age))

plot4 <- ggplot(combinededited, aes(x = Age, y = Freq, fill = Group)) +
  geom_area() +
  ylim(0, 100) +
  scale_x_continuous(breaks = c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)) +
  ylab("Cumulative Percentage Covered")

plot4 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.title = element_text(size = 16),
                           axis.text = element_text(size = 14),
                           legend.text = element_text(size = 14),
                           legend.title = element_text(size = 16))

