install.packages("dplyr")
require(dplyr)
require(ggplot2)
library(ggplot2)
library(dplyr)
require(ggthemes)
library(ggthemes)

rm(list = ls())

load("../data/KeyWestAnnualMeanTemperature.RData")
png(file = "../data/ats_plot.png")
ggplot(ats, aes(Year, Temp)) + 
  xlab("Year in 20th century") + 
  ylab("Temperature") +
  geom_point(shape = I(4)) +
  geom_smooth(method = "lm", fullrange = T, colour = "red") +
  geom_text(
    aes(x = median(Year), y = max(Temp) + 0.3, 
        label = "Appropriate Correlation Coefficient ≈ 0.533"),
        size = 2,
        colour = "blue") +
  theme_bw()
dev.off()

acc_Y_T <- cor.test(ats$Year, ats$Temp, use = "pairwise")
acc_1 <- acc_Y_T$estimate

cor_f1 <- function(data){
  r <- cor.test(ats$Year, sample(ats$Temp, length(ats$Temp), replace = FALSE), use = "pairwise")
  return(r$estimate)
}
c_c_values_in_10000_sample <- as.data.frame(sapply(1:10000, function(x) cor_f1(ats)))
colnames(c_c_values_in_10000_sample) = "c_c_values"

pos <- sum(c_c_values_in_10000_sample > acc_1) / 10000
pos

png(file = "../data/random_ats_plot.png")
ggplot(c_c_values_in_10000_sample, aes(x = c_c_values)) +
  labs(title = "The Distribution of Random Correlation Coefficients") +
  xlab("Correlation Coefficient") + 
  geom_density() +
  geom_vline(xintercept = 0.533, linetype = "dashed", color = "red") +
  theme_bw()
dev.off()

