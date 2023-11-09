rm(list = ls())
MyDF <- as.data.frame(read.csv("../data/EcolArchives-E089-51-D1.csv"))
require(tidyverse)
library(tidyverse)
require(ggplot2)
library(ggplot2)
require(ggthemes)
library(ggthemes)


p <- ggplot(MyDF,
       aes(
         Prey.mass,
         Predator.mass,
         colour = Predator.lifestage)) +
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


lm_models_data <- MyDF %>%
  group_by(Type.of.feeding.interaction, Predator.lifestage) %>%
  do(model = lm(Predator.mass ~ Prey.mass, data = .)) %>%
  reframe(
    Type.of.feeding.interaction,
    Predator.lifestage,
    intercept = summary(model)$coefficients[1],
    slope = summary(model)$coefficients[2],
    r_squared = summary(model)$r.squared,
    f_statistic = as.numeric(summary(model)$fstatistic[1]),
    p_value = summary(model)$coefficients[, "Pr(>|t|)"][2]
  )

write.csv(lm_models_data, "../results/PP_Regress_Results.csv", row.names = F)






