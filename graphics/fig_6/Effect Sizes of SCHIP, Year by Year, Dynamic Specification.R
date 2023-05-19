library(readxl)
thesisRegressions <- read_excel("Thesis Regressions.xlsx", sheet = "SCHIP 3")

library(tidyverse)

coverage <- thesisRegressions[, c("Any Coverage", "Coefficient")]

anyCov <- coverage[1:17,]
pubCov <- coverage[20:36,]
privCov <- coverage[39:54,]

anyCov$"Any Coverage" <- as.integer(anyCov$"Any Coverage")
pubCov$"Any Coverage" <- as.integer(pubCov$"Any Coverage")
privCov$"Any Coverage" <- as.integer(privCov$"Any Coverage")

anyCov$Coefficient <- as.numeric(anyCov$Coefficient)
pubCov$Coefficient <- as.numeric(pubCov$Coefficient)
privCov$Coefficient <- as.numeric(privCov$Coefficient)

anyCov$Coefficient <- anyCov$Coefficient * 100
pubCov$Coefficient <- pubCov$Coefficient * 100
privCov$Coefficient <- privCov$Coefficient * 100

anyCov <- anyCov %>% rename(Year = "Any Coverage", "Any" = Coefficient)
pubCov <- pubCov %>% rename(Year = "Any Coverage", "Public" = Coefficient)
privCov <- privCov %>% rename(Year = "Any Coverage", "Private" = Coefficient)

combined <- merge(anyCov, pubCov, by = "Year")
combined <- merge(combined, privCov, by = "Year")

plot6 <- ggplot(combined, aes(x = Year)) +
  geom_line(aes(y = Any, color = "Any"), group = 1) +
  geom_line(aes(y = Public, color = "Public"), linetype = "twodash", group = 1) + 
  geom_line(aes(y = Private, color = "Private"), linetype = "dashed", group = 1) +
  geom_hline(yintercept=0)+ 
  ylab("Impact") + 
  scale_x_continuous(limits = c(1991, 2007), breaks=c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008)) + 
  scale_y_continuous(limits = c(-2, 6), breaks=c(-2, -1, 0, 1, 2, 3, 4, 5, 6)) + 
  scale_color_manual("Type of Coverage", breaks = c("Any", "Public", "Private"), values = c("red", "blue", "darkgreen"))

plot6 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                            axis.title = element_text(size = 18),
                            axis.text = element_text(size = 15),
                            legend.text = element_text(size = 16),
                            legend.title = element_text(size = 18))
