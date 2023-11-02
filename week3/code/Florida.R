install.packages("dplyr")
require(dplyr)
library(dplyr)
rm(list = ls())

load("../data/KeyWestAnnualMeanTemperature.RData")

acc_Y_T <- cor.test(ats$Year, ats$Temp, use = "pairwise")
acc_1 <- acc_Y_T$estimate
pdf("../results/ats_p.pdf")
print(plot(ats))
dev.off()

cor_f1 <- function(data){
  r <- cor.test(ats$Year, sample(ats$Temp, length(ats$Temp), replace = FALSE), use = "pairwise")
  return(r$estimate)
}
c_c_values_in_10000_sample <- as.data.frame(sapply(1:10000, function(x) cor_f1(ats)))
colnames(c_c_values_in_10000_sample) = "c_c_values"

pos <- sum(c_c_values_in_10000_sample > acc_1) / 10000
pos


