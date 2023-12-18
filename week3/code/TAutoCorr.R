#Author: PU ZHAO pu.zhao23@imperial.ac.uk
#Script: TAutoCorr.R
#Desc: Analyze whether the temperature in one year is significantly related to the temperature in the next year (consecutive years) at a given location.

require(dplyr)
require(ggplot2)
library(ggplot2)
library(dplyr)
require(ggthemes)
library(ggthemes)

set.seed(1)
rm(list = ls())
load("../data/KeyWestAnnualMeanTemperature.RData")
cor_f1 <- function(data){
  num <- length(data$Year)
  temp1 <- data$Temp[-num]
  temp2 <- data$Temp[-1]
  c_c_in_successive_years <- cor(temp1, temp2, use = "pairwise")
  return(c_c_in_successive_years)
}

cor_f2 <- function(data){
  data$Temp <- sample(data$Temp, length(data$Temp), replace = FALSE)
  num <- length(data$Year)
  temp1 <- data$Temp[-num]
  temp2 <- data$Temp[-1]
  c_c_in_successive_years <- cor(temp1, temp2, use = "pairwise")
  return(c_c_in_successive_years)
}
c_c_1 <- cor_f1(ats)
c_c_values_10000 <- as.data.frame(sapply(1:10000, function(x) cor_f2(ats)))
colnames(c_c_values_10000) = "c_c_values"
pos <- sum(c_c_values_10000 > c_c_1) / 10000


num <- length(ats$Year)
Temp1 <- ats$Temp[-1] - ats$Temp[-num]
png(file = "../results/ats_plot.png")
df_temp <- data.frame(Temp1 = ats$Temp[-num], Temp2 = ats$Temp[-1])
gg <- ggplot(df_temp, aes(Temp1, Temp2)) +
  xlab("Temperature1") + 
  ylab("Temperature2") +
  geom_point(shape = I(4)) +
  geom_smooth(method = "lm", fullrange = T, colour = "red") +
  xlim(c(23.5, 26.5)) + 
  ylim(c(23.5, 26.5)) + 
  theme_bw()
print(gg)
dev.off()

png(file = "../results/ats_random_plot.png")
g <- ggplot(c_c_values_10000, aes(x = c_c_values)) +
  xlab("Correlation Coefficient") + 
  geom_histogram(fill = "lightblue", colour = "black")+
  geom_vline(xintercept = c_c_1, linetype = "dashed", color = "red") +
  theme_bw()
print(g)
dev.off()









