# CMEE 2022 HPC exercises R code pro forma
# For stochastic demographic model cluster run

rm(list=ls())
graphics.off()
# source("/Users/zpu/CMEECourseWork/HPC/pz123_HPC_2023_main.R")
source("pz123_HPC_2023_main.R")
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
# iter <- 66
set.seed(iter)

clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
simulation_length <- 120
growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                          0.5, 0.4, 0.0, 0.0,
                          0.0, 0.4, 0.7, 0.0,
                          0.0, 0.0, 0.25, 0.4),
                        nrow=4, ncol=4, byrow=T)
reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0),
                              nrow=4, ncol=4, byrow=T)
init_state_1 <- state_initialise_adult(4,100)
init_state_2 <- state_initialise_adult(4,10)
init_state_3 <- state_initialise_spread(4, 100)
init_state_4 <- state_initialise_spread(4,10)


if(iter >= 1 & iter <= 25){
  initial_state <- init_state_1
}else if(iter > 25 & iter <= 50){
  initial_state <- init_state_2
}else if(iter > 50 & iter <= 75){
  initial_state <- init_state_3
}else if(iter > 75 & iter <= 100){
  initial_state <- init_state_4
}

# ## test
# for(i in 1:150){
#   pop_size <- stochastic_simulation(
#     initial_state = init_state_4,
#     growth_matrix = growth_matrix,
#     reproduction_matrix = reproduction_matrix,
#     clutch_distribution = clutch_distribution,
#     simulation_length = simulation_length
#   )  
#   allpop_list <- c(allpop_list,list(pop_size))
# }
# k <- 0
# for(list in allpop_list){
#   if(list[length(list)] == 0){
#     print(list[length(list)])
#     k <- k + 1
#   } 
# }
# print(k)

allpop_list <- list()
for(i in 1:150){
  pop_size <- stochastic_simulation(
    initial_state = initial_state,
    growth_matrix = growth_matrix,
    reproduction_matrix = reproduction_matrix,
    clutch_distribution = clutch_distribution,
    simulation_length = simulation_length
  )  
  allpop_list <- c(allpop_list,list(pop_size))
}
save(allpop_list, file = paste("pz123_2_", iter, ".rda", sep=""))



