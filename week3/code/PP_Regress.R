rm(list = ls())
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
require(tidyverse)
library(tidyverse)
require(ggplot2)
library(ggplot2)
library(ggthemes)


p <- ggplot(MyDF,
       aes(
         Prey.mass,
         Predator.mass,
         colour = Predator.lifestage),
         ) +
  xlab("Prey Mass in grams") + 
  ylab("Predator Mass in grams") +
  facet_grid(Type.of.feeding.interaction ~.) +
  geom_point(shape = I(3)) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10(breaks = c(1e-06, 1e-02, 1e+02, 1e+06)) +
  geom_smooth(method = "lm", fullrange = T) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face = "bold", size = 8)) +
  guides(color = guide_legend(nrow = 1)) +
  theme(aspect.ratio=0.5)
pdf("../results/PP_Regress.pdf")
print(p)
dev.off()


lm1 <- MyDF %>% group_by(Type.of.feeding.interaction, Predator.lifestage)
lm1 <- summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass), data = subdata)
intercept <- lm1$coefficients[1]
slope <- lm1$coefficients[2]
r_squared <- lm1$r.squared
f_statistic <- lm1$fstatistic[1]
p_value <- lm1$coefficients[, "Pr(>|t|)"][2]

regression_results <- data.frame(
  "The regression results" = c("intercept", "slope", "r_squared", "f_statistic", "p_value"),
  "Value" = c(intercept, slope, r_squared, f_statistic, p_value)
)

write.csv(regression_results, "../results/PP_Regress_Results.csv", row.names = F)






