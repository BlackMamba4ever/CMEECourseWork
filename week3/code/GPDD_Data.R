rm(list = ls())
load("../data/GPDDFiltered.RData")
require(MAP)
require(ggplot2)
library(ggplot2)
library(MAP)

w_map <- map_data("world")
gpdd_w_map <- ggplot() + 
  geom_polygon(
    data = w_map,
    aes(x = long, y = lat, group = group),
    fill = "lightyellow",
    colour = "lightblue"
  ) +
  geom_text(
    data = gpdd,
    aes(x = long, y = lat, label = common.name),
    size = 3,
    colour = "black"
  ) +
  geom_point(
    data = gpdd,
    aes(x = long, y = lat, group = common.name),
    colour = "red",
    size = 0.8
  ) +
  labs(title = "World Map with GPDD_Data")
print(gpdd_w_map)

# Possible biases in the analysis based on the data represented:
# 1. Selection Bias: The map only shows data points available in the GPDD_Data. This may not be representative of global situation.
# 2. Sampling Bias: The data points on the graph are mostly concentrated in Europe and North America, which may lead to an over-analysis of these locations, potentially distorting the global analysis and not accurately representing the global analysis.
# 3. Missing Data Bias: Data for many regions are missing from the data, and the analysis results may be biased by missing data. This may result in incomplete or inaccurate results.
# 4. Data Quality: There may be minor errors in the accuracy of the data, resulting in errors in the overall global analysis.
# 5. Temporal Bias: The map does not account for temporal changes in the data. Analysis may need to consider how data distributions have changed over time.

