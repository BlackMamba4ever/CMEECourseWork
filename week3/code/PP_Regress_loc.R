#Author: PU ZHAO pu.zhao23@imperial.ac.uk
#Script: PP_Regress_loc.R
#Desc: A script for illustrating predator and prey body mass distributions and saving summary results


require(tidyverse)
rm(list=ls())
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# Convert mg values to g (does this in existing column)
MyDF$Prey.mass[MyDF$Prey.mass.unit == 'mg'] = MyDF$Prey.mass[MyDF$Prey.mass.unit == 'mg'] * 1000

# Convert to factors
MyDF$Type.of.feeding.interaction <- as.factor(MyDF$Type.of.feeding.interaction)
MyDF$Location <- as.factor(MyDF$Location) 
MyDF$Predator.lifestage <- as.factor(MyDF$Predator.lifestage)

# Subset MyDF based on Feeding Interaction and Predator Lifestage
MyDF.subsets = group_split(MyDF, Location, Type.of.feeding.interaction, Predator.lifestage)

# Create linear model function
lm.function = function(x){
  model = lm(log(Predator.mass) ~ log(Prey.mass), data = x) # Linear model
  slopes = coef(model)[[2]] # Slopes
  intercepts = coef(model)[[1]] # Intercepts 
  r2 = summary(model)$r.squared # R-squared values
  Fstats = ifelse(is.null(summary(model)$fstatistic[[1]]), NA, summary(model)$fstatistic[[1]]) # F-statistics (accounting for NA's)
  pvalues = ifelse(nrow(summary(model)$coef) > 1, summary(model)$coef[2,4], NA) # P-Values 
  output = data.frame(Location = x$Location[[1]], # Create an output dataframe for each subset
                      Predator.lifestage = x$Predator.lifestage[[1]],
                      FeedingInteraction = x$Type.of.feeding.interaction[[1]],
                      slope = slopes,
                      intercept = intercepts,
                      r2 = r2,
                      fstat = Fstats,
                      pvalue = pvalues)
  return(output)
}

PP_Regress_Results <- bind_rows(lapply(MyDF.subsets, lm.function))
write.csv(na.omit(PP_Regress_Results),file="PP_Regress_Results.csv")
print("Model summary file outputted to: ../results/PP_Regress_Results.csv")

