library(ipumsr)
library(tidyverse)
cps_ddi_file <- "cps_00006.xml"
cps_data_file <- "cps_00006.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

df <- as.data.frame(cps_data)
cpsdf <- subset(df, YEAR <= 2008)
cpsdf <- subset(cpsdf, AGE <= 30)
cpsdf <- subset(cpsdf, AGE >= 14)
cpsdf <- cpsdf[, c("AGE", "RELATE")]

#0301 is child

tbl <- table(cpsdf)
tbl <- tbl / rowSums(tbl) * 100

tbldf <- as.data.frame(tbl)
withParents <- tbldf %>% filter(tbldf$RELATE == 301)
withParents <- withParents[, c("AGE", "Freq")]

withParents <- withParents %>% rename(Age = AGE)

plot3 <- ggplot(withParents, aes(x = Age)) + 
  geom_line(aes(y = Freq, color = "Red"), group = 1) +
  ylab("Percent") + 
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

plot3 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.position = "none",
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.title = element_text(size = 16),
                           axis.text = element_text(size = 14))
