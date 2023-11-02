rm(list = ls())
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
require(tidyverse)
MyDF <-
  MyDF %>% mutate(
    Type.of.feeding.interaction = as.factor(Type.of.feeding.interaction),
    Location = as.factor(Location)
  )

# Subplots of distributions of predator mass.
pdf("../results/Pred_Subplots.pdf", 11.7, 8.3)
hist(
  log10(MyDF$Predator.mass),
  main = "Pred_Subplots",
  xlab = "log10(Predator Mass (g))",
  ylab = "Count",
  col = "yellow",
  border = "black"
)

# Subplots of distributions of prey mass.
pdf("../results/Prey_Subplots.pdf", 11.7, 8.3)
hist(
  log10(MyDF$Prey.mass),
  main = "Prey_Subplots",
  xlab = "log10(Prey Mass (g))",
  ylab = "Count",
  col = "lightblue",
  border = "black"
)

# The size ratio of prey mass over predator mass by feeding interaction type.
Data_by_tofi <- MyDF %>% group_by(Type.of.feeding.interaction)
num_group <- Data_by_tofi %>% summarise(num_tofi = n())
sub_tofi <-
  Data_by_tofi %>% select(Type.of.feeding.interaction, Predator.mass, Prey.mass)
sub_split_tofi <- group_split(sub_tofi)
pdf("../results/SizeRatio_Subplots.pdf", 11.7, 8.3)
par(mfrow = c(2, 3))

i = 1
for (data_frame in sub_split_tofi) {
  cols = c("lightblue", "yellow", "lightpink", "lightgreen", "grey")
  p <- paste("Feeding interaction:", data_frame[2, 1])
  hist(
    log10(data_frame$Predator.mass / data_frame$Prey.mass),
    main = p,
    xlab = "log10(Predator.mass / Prey.mass)",
    ylab = "Count",
    col = cols[i],
    border = "black"
  )
  i = i + 1
}

PP_csv_Results <-
  sub_tofi %>% summarise(
    Type.of.feeding.interaction = unique(Type.of.feeding.interaction),
    mean_pred = mean(log10(Predator.mass)),
    mean_prey = mean(log10(Prey.mass)),
    median_pred = median(log10(Predator.mass)),
    median_prey = median(log10(Prey.mass)),
    mean_pred_prey_size_ratio = mean(log10(Predator.mass/Prey.mass)),
    median_pred_prey_size_ratio = median(log10(Predator.mass/Prey.mass))
  )

write.csv(PP_csv_Results, file = "../results/PP_Results.csv", row.names = FALSE)
graphics.off()
