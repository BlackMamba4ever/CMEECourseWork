# CMEE 2023 HPC exercises R code pro forma
# For neutral model cluster run

rm(list = ls())
graphics.off()
source("pz123_HPC_2023_main.R")
# source("/Users/zpu/CMEECourseWork/HPC/pz123_HPC_2023_main.R")
# iter <- 1
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

set.seed(iter)
init_commy <- c(500, 1000, 2500, 5000)
if (iter >= 1 & iter <= 25) {
  size <- init_commy[1]
}
if (iter > 25 & iter <= 50) {
  size <- init_commy[2]
}
if (iter > 50 & iter <= 75) {
  size <- init_commy[3]
}
if(iter > 75 & iter <= 100){
  size <- init_commy[4]
}
file_name = paste("pz123_1_", iter, ".rda", sep = "")
neutral_cluster_run(
  speciation_rate = 0.003599,
  size = size,
  wall_time = 11.5*60,
  interval_rich = 1,
  interval_oct = size / 10,
  burn_in_generations = size * 8,
  output_file_name = file_name
)
