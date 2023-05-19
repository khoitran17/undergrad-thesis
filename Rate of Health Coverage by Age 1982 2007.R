library(ipumsr)
library(tidyverse)
cps_ddi_file <- "cps_00014.xml"
cps_data_file <- "cps_00014.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

df <- as.data.frame(cps_data)

healthcovdf <- df[, c("YEAR", "AGE", "UH_CHAMP_A1", "UH_COVGH_A1", "UH_COVHI_A1", "UH_HVHI_A1", "UH_MCAID_A1")]

healthcovdf <- healthcovdf %>% mutate(hascoverage = case_when(UH_CHAMP_A1 == 1 | UH_COVGH_A1 == 1 | UH_HVHI_A1 == 1| UH_MCAID_A1 == 1 ~ 1))

healthcovdf1982 <- healthcovdf[, c("AGE", "hascoverage")]

healthcovdf1982[is.na(healthcovdf1982)] <- 0

tbl1982 <- table(healthcovdf1982)
tbl1982 <- tbl1982 / rowSums(tbl1982) * 100
tbl1982df <- as.data.frame(tbl1982)

hasinsurance1982 <- tbl1982df %>% filter(tbl1982df$hascoverage == 1)
hasinsurance1982 <- as.data.frame(hasinsurance1982)
hasinsurance1982 <- hasinsurance1982[, c("AGE", "Freq")]
hasinsurance1982 <- hasinsurance1982 %>% rename(Age = AGE, Percent1982 = Freq)
hasinsurance1982 <- subset(hasinsurance1982, as.numeric(Age) < 52)

cps_ddi_file1 <- "cps_00002.xml"
cps_data_file1 <- "cps_00002.dat"
cps_ddi1 <- read_ipums_ddi(cps_ddi_file1)
cps_data1 <- read_ipums_micro(cps_ddi_file1, data_file = cps_data_file1)

df1 <- as.data.frame(cps_data1)

healthcovdf1 <- df1[, c("YEAR", "AGE", "HCOVANY")]
healthcov2007 <- healthcovdf1 %>% filter(YEAR == 2007)
healthcov2007 <- healthcov2007[, c("AGE", "HCOVANY")]

tbl2007 <- table(healthcov2007)
tbl2007 <- tbl2007 / rowSums(tbl2007) * 100
tbl2007df <- as.data.frame(tbl2007)
hasinsurance2007 <- tbl2007df %>% filter(tbl2007df$HCOVANY == 2)
hasinsurance2007 <- as.data.frame(hasinsurance2007)
hasinsurance2007 <- hasinsurance2007[, c("AGE", "Freq")]
hasinsurance2007<- hasinsurance2007 %>% rename(Age = AGE, Percent2007 = Freq)
hasinsurance2007 <- subset(hasinsurance2007, as.numeric(Age) < 52)

combined <- merge(hasinsurance1982, hasinsurance2007, by = "Age")


plot1 <- ggplot(combined, aes(x = Age)) + 
          geom_line(aes(y = Percent2007, color = "2007"), group = 1) +
          geom_line(aes(y = Percent1982, color = "1982"), linetype = "twodash", group = 1) + 
          ylim (50, 100) + 
          ylab("Percent Covered by Health Insurance") + 
          scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]}) +
          scale_color_manual("Year", labels = c("1982", "2007"), values = c("red", "blue"))

plot1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          legend.title = element_text(size = 16))
