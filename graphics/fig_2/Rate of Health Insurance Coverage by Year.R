library(ipumsr)
library(tidyverse)
cps_ddi_file <- "cps_00003.xml"
cps_data_file <- "cps_00003.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

df <- as.data.frame(cps_data)

cpsdf <- df[, c("YEAR", "AGE", "HCOVANY")]
cpsdf <- subset(cpsdf, YEAR <= 2008)

cpsdf1617 <- subset(cpsdf,  AGE == 16 | AGE == 17)
cpsdf2021 <- subset(cpsdf,  AGE == 20 | AGE == 21)

tbl1617 <- table(cpsdf1617[, c("YEAR", "HCOVANY")])
tbl2021 <- table(cpsdf2021[, c("YEAR", "HCOVANY")])

tbl1617 <- tbl1617 / rowSums(tbl1617) * 100
tbl2021 <- tbl2021 / rowSums(tbl2021) * 100

tbl1617df <- as.data.frame(tbl1617)
tbl2021df <- as.data.frame(tbl2021)

hasinsurance1617 <- tbl1617df %>% filter(tbl1617df$HCOVANY == 2)
hasinsurance2021 <- tbl2021df %>% filter(tbl2021df$HCOVANY == 2)

hasinsurance1617 <- hasinsurance1617[, c("YEAR", "Freq")]
hasinsurance2021 <- hasinsurance2021[, c("YEAR", "Freq")]

hasinsurance1617 <- hasinsurance1617 %>% rename(Year = YEAR, Percent1617 = Freq)
hasinsurance2021 <- hasinsurance2021 %>% rename(Year = YEAR, Percent2021 = Freq)

combined <- merge(hasinsurance1617, hasinsurance2021, by = "Year")

plot2 <- ggplot(combined, aes(x = Year)) + 
          geom_line(aes(y = Percent1617, color = "16-17"), group = 1) +
          geom_line(aes(y = Percent2021, color = "20-21"), linetype = "twodash", group = 1) + 
          ylab("Percent Covered by Health Insurance") + 
          ylim(50, 100) + 
          scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]}) + 
          scale_color_manual("Age", breaks = c("16-17", "20-21"), values = c("red", "blue"))

plot2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.title = element_text(size = 16),
                           axis.text = element_text(size = 14),
                           legend.text = element_text(size = 14),
                           legend.title = element_text(size = 16))
