# CMEE 2023 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "PU ZHAO"
preferred_name <- "PU"
email <- "pz123@imperial.ac.uk"
username <- "pz123"

# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!
rm(list = ls())
graphics.off()
# Question 1
species_richness <- function(community){
  richness <-length(unique(community))
  return(richness)
}

# Question 2
init_community_max <- function(size){
  init_community <- seq(1,size,1)
  return(init_community)
}

# Question 3
init_community_min <- function(size){
  init_community <- rep(1,size)
  return(init_community)
}

# Question 4
choose_two <- function(max_value){
  c_t_values <- sample(1:max_value,2,F)
  return(c_t_values)
}

# Question 5
neutral_step <- function(commy){
  two_species <- choose_two(length(commy))
  commy[two_species[1]] <- commy[two_species[2]]
  return(commy)
}

# Question 6
neutral_generation <- function(commy){
  generation <- round(species_richness(commy) / 2)
  for(i in 1:generation){
    commy <- neutral_step(commy)
  }
  return(commy)
}

# Question 7
neutral_time_series <- function(commy, duration){
  richness <- c(species_richness(commy))
  for(i in 1:duration){
    commy <- neutral_generation(commy)
    richness <- c(richness, species_richness(commy))
  }
  return(richness)
}

# Question 8
question_8 <- function(commy_num, duration){
  png("question_8.png", width = 600, height = 400)
  richness <- neutral_time_series(init_community_max(100), duration = 200)
  time <- seq(0, 200, 1)
  plot(time, richness, type = "l", xlab = "time", ylab = "richness", main = "Time Series Graph of Neutral Model Simulation")
  Sys.sleep(0.1)
  dev.off()
  return(cat(paste("After waiting for a sufficiently long time, the system will converge to a state where only a single species remains.", " \n\nThis is because, in this model, individuals are continuously replaced by others, leading to the gradual extinction of species until only one species is left.", sep = "")))
}
question_8()
# Question 9
neutral_step_speciation <- function(commy, speciation_rate){
  two_species <- choose_two(length(commy))
  if(runif(1) <= speciation_rate){
    new_species <- max(commy) + 1
    commy[two_species[1]] <- new_species
    return(commy)
  }else{
    commy[two_species[1]] <- commy[two_species[2]]
    return(commy)
  }
}

# Question 10
neutral_generation_speciation <- function(commy, speciation_rate){
  generation <- round(species_richness(commy) / 2)
  for(i in 1:generation){
    commy <- neutral_step_speciation(commy, speciation_rate)
  }
  return(commy)
}

# Question 11
neutral_time_series_speciation <- function(commy, duration, speciation_rate){
  richness <- c(species_richness(commy))
  for(i in 1:duration){
    commy <- neutral_generation_speciation(commy, speciation_rate)
    richness <- c(richness, species_richness(commy))
  }
  return(richness)
}

# Question 12
question_12 <- function(){
  png("question_12.png", width = 600, height = 400)
  richness_1 <- neutral_time_series_speciation(init_community_max(100), duration = 200, speciation_rate = 0.1)
  richness_2 <- neutral_time_series_speciation(init_community_min(100), duration = 200, speciation_rate = 0.1)
  time <- seq(0, 200, 1)

  
  g <- ggplot() + 
    geom_line(aes(x = time, y = richness_1, color = "richness_max")) + 
    geom_line(aes(x = time, y = richness_2, color = "richness_min")) + 
    xlab("time") + 
    ylab("richness") + 
    ggtitle("Time Series Graph of Neutral Model Simulation")
  print(g)
  Sys.sleep(0.1)
  dev.off()
  return(cat(paste("Whether the initial community consists solely of a single species or contains the maximum number of species, they eventually converge to the same point, which is the maximum number of species present in the initial conditions.", "\n\nThis is because the introduction of a new species is accompanied by a reduction of one of the original species. For a community originally consisting of 100 unique species, each species represented by a single individual, the species richness remains unchanged. However, for a community that initially has only one species with 100 individuals, the species richness increases continually until it finally reaches the maximum species richness, thus becoming akin to the first scenario.")))
}
# Question 13
species_abundance <- function(commy){
  tb_rich <- as.vector(table(commy))
  spc_abd <- sort(tb_rich, decreasing = T)
  return(spc_abd)
}

# Question 14
octaves <- function(commy){
  tbu <- tabulate(species_abundance(commy))
  oct <- c()
  num_oct <- floor(log2(length(tbu))) + 1 ## 一共有几个度
  for(i in 1:num_oct){
    count <- 0
    for(j in 1:length(tbu)){
      if(j >= 2^(i-1) & j < 2^i){
        count <- count + tbu[j]
      }
    }
    oct <- c(oct, count)
  }
  return(oct)
}

# Question 15
sum_vect <- function(v1, v2) {
  len_diff <- length(v1) - length(v2)
  if (len_diff > 0) {
    v2_new <- c(v2, rep(0, len_diff))
    v <- v1 + v2_new
  } else if (len_diff < 0) {
    v1_new <- c(v1, rep(0, -len_diff))
    v <- v1_new + v2
  } else{
    v <- v1 + v2
  }
  return(v)
}

# Question 16 
question_16 <- function(){
  commy_min <- init_community_min(100)
  commy_max <- init_community_max(100)
  for(i in 1:200){
    commy_min <- neutral_generation_speciation(commy_min, speciation_rate = 0.1)
    commy_max <- neutral_generation_speciation(commy_max, speciation_rate = 0.1)
  }
  oct_rich_min <- octaves(commy_min)
  oct_rich_max <- octaves(commy_max)
  for(i in 1:2000){
    commy_min <-neutral_generation_speciation(commy_min, speciation_rate = 0.1)
    commy_max <-neutral_generation_speciation(commy_max, speciation_rate = 0.1)
    if(i %% 20 == 0){
      oct_rich_min <- sum_vect(oct_rich_min, octaves(commy_min))
      oct_rich_max <- sum_vect(oct_rich_max, octaves(commy_max))
    }
  }
  oct_rich_min_mean <- oct_rich_min / 100
  oct_rich_max_mean <- oct_rich_max / 100
  png("question_16_plot_min.png", width = 600, height = 400)
  barplot(oct_rich_min_mean, col = "lightblue", border = "black", main = "Bar plot of species abundance distribution in octaves(min)")
  Sys.sleep(0.1)
  dev.off()
  png("question_16_plot_max.png", width = 600, height = 400)
  barplot(oct_rich_max_mean, col = "lightpink", border = "black", main = "Bar plot of species abundance distribution in octaves(max)")
  Sys.sleep(0.1)
  dev.off()
  return(cat("Initial conditions are not important. Their speciation rates are the same, and their running generation lengths are the same, so they have the same pattern."))
}
#question_16()
# Question 17
neutral_cluster_run <-
  function(speciation_rate,
           size,
           wall_time,
           interval_rich,
           interval_oct,
           burn_in_generations,
           output_file_name) {
    commy_min <- init_community_min(size)
    wall_time_sec <- wall_time * 60
    s_time <- proc.time()[[3]]
    used_time <- proc.time()[[3]] - s_time
    richness_time_series <- c()
    octaves_min <- list()
    generation <- 1
    while (used_time < wall_time_sec) {
      commy_min <-
        neutral_generation_speciation(commy_min, speciation_rate)
      if (generation %% interval_rich == 0 &&
          generation < burn_in_generations) {
        richness_time_series <- c(richness_time_series, species_richness(commy_min))
      }
      if (generation %% interval_oct == 0) {
        octaves_min[[length(octaves_min) + 1]] <- octaves(species_abundance(commy_min))
      }
      generation <- generation + 1
      used_time <- proc.time()[[3]] - s_time
    }
    total_time <- proc.time()[[3]] - s_time
    save(
      commy_min,
      richness_time_series,
      octaves_min,
      total_time,
      speciation_rate,
      size,
      wall_time,
      interval_rich,
      interval_oct,
      burn_in_generations,
      file = output_file_name
     )
  }

# neutral_cluster_run(
#   speciation_rate = 0.1,
#   size = 1000,
#   wall_time = 5,
#   interval_rich = 1,
#   interval_oct = 1000 / 10,
#   burn_in_generations = 1000 * 8,
#   output_file_name = paste("pz123_1_", 26, ".rda", sep = "")
# )
# neutral_cluster_run(
#   speciation_rate = 0.1,
#   size = 1500,
#   wall_time = 10,
#   interval_rich = 1,
#   interval_oct = 1500 / 10,
#   burn_in_generations = 1500 * 8,
#   output_file_name = paste("pz123_1_", 51, ".rda", sep = "")
# )
# neutral_cluster_run(
#   speciation_rate = 0.1,
#   size = 2000,
#   wall_time = 15,
#   interval_rich = 1,
#   interval_oct = 2000 / 10,
#   burn_in_generations = 2000 * 8,
#   output_file_name = paste("pz123_1_", 76, ".rda", sep = "")
# )
# neutral_cluster_run_test <-
#   function(speciation_rate,
#            size,
#            wall_time,
#            interval_rich,
#            interval_oct,
#            burn_in_generations,
#            output_file_name) {
#     wall_time = 0.1
#     speciation_rate = 0.1
#     output_file_name <- paste("pz123_1_", iter, ".rda", sep = "")
#     interval_rich <- 1
#     interval_oct = size / 10
#     burn_in_generations = size * 8
#     commy_min <- init_community_min(500)
#     wall_time_sec <- wall_time * 60
#     s_time <- proc.time()[[3]]
#     used_time <- proc.time()[[3]] - s_time
#     richness_time_series <- c()
#     octaves_min <- list()
#     generation <- 1
#     while (used_time < wall_time_sec) {
#       commy_min <-
#         neutral_generation_speciation(commy_min, speciation_rate)
#       if (generation %% interval_rich == 0 &&
#           generation < burn_in_generations) {
#         richness_time_series <- c(richness_time_series, species_richness(commy_min))
#       }
#       if (generation %% interval_oct == 0) {
#         c(octaves_min, list(octaves(species_abundance(commy_min))))
#       }
#       generation <- generation + 1
#       used_time <- proc.time()[[3]] - s_time
#     }
#     total_time <- proc.time()[[3]] - s_time
#     print(total_time)
#     save(
#       commy_min,
#       richness_time_series,
#       octaves_min,
#       total_time,
#       speciation_rate,
#       size,
#       wall_time,
#       interval_rich,
#       interval_oct,
#       burn_in_generations,
#       file = output_file_name
#     )
#   }

# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  # load combined_results from your rda file
  rda_files <- list.files(pattern = "^pz123_1_.*\\.rda$")
  oct_500 <- c()
  oct_1000 <- c()
  oct_1500 <- c()
  oct_2000 <- c()
  gab_500 <- 0
  gab_1000 <- 0
  gab_1500 <- 0
  gab_2000 <- 0

  for (file in rda_files) {
    load(file)
    if (size == 1000) {
      print(octaves_min)
      for (i in 1:length(octaves_min)) {
        if (i > burn_in_generations / interval_oct) {
          oct_1000 <- sum_vect(octaves_min[[i]], oct_1000)
        }
      }
      gab_1000 <-
        gab_1000 + length(octaves_min) - burn_in_generations / interval_oct
    }
  }
  for (file in rda_files) {
    load(file)
    if (size == 500) {
      print(octaves_min)
      for (i in 1:length(octaves_min)) {
        if (i > burn_in_generations / interval_oct) {
          oct_500 <- sum_vect(octaves_min[[i]], oct_500)
        }
      }
      gab_500 <-
        gab_500 + length(octaves_min) - burn_in_generations / interval_oct
    }
    if (size == 1000) {
      print(octaves_min)
      for (i in 1:length(octaves_min)) {
        if (i > burn_in_generations / interval_oct) {
          oct_1000 <- sum_vect(octaves_min[[i]], oct_1000)
        }
      }
      gab_1000 <-
        gab_1000 + length(octaves_min) - burn_in_generations / interval_oct
    }
    if (size == 1500) {
      for (i in 1:length(octaves_min)) {
        if (i > burn_in_generations / interval_oct) {
          oct_1500 <- sum_vect(octaves_min[[i]], oct_1500)
        }
      }
      gab_1500 <-
        gab_1500 + length(octaves_min) - burn_in_generations / interval_oct
    }
    if (size == 2000) {
      for (i in 1:length(octaves_min)) {
        if (i > burn_in_generations / interval_oct) {
          oct_2000 <- sum_vect(octaves_min[[i]], oct_2000)
        }
      }
      gab_2000 <-
        gab_2000 + length(octaves_min) - burn_in_generations / interval_oct
    }
  }

  size_500_mean <- oct_500 / gab_500
  size_1000_mean <- oct_1000 / gab_1000
  size_1500_mean <- oct_1500 / gab_1500
  size_2000_mean <- oct_2000 / gab_2000
  combined_results <- list(
    size_500_mean,
    size_1000_mean,
    size_1500_mean,
    size_2000_mean
  )
  # save results to an .rda file
  save(combined_results, file = "combined_results.rda")
}

# process_neutral_cluster_results()

plot_neutral_cluster_results <- function() {
  graphics.off()
  #load the data
  load("combined_results.rda")
  size_500_mean <- combined_results[[1]]
  size_1000_mean <- combined_results[[2]]
  size_1500_mean <- combined_results[[3]]
  size_2000_mean <- combined_results[[4]]

  png(filename = "plot_neutral_cluster_results.png",
      width = 600,
      height = 400)
  # plot your graph here
  par(mfrow = c(2, 2))
  barplot(
    size_500_mean,
    xlab = "Octaves",
    ylab = "Richness",
    main = "Community: 500",
    col = "lightpink"
  )
  barplot(
    size_1000_mean,
    xlab = "Octaves",
    ylab = "Richness",
    main = "Community: 1000",
    col = "lightblue"
  )
  barplot(
    size_1500_mean,
    xlab = "Octaves",
    ylab = "Richness",
    main = "Community: 1500",
    col = "lightyellow"
  )
  barplot(
    size_2000_mean,
    xlab = "Octaves",
    ylab = "Richness",
    main = "Community: 2000",
    col = "lightgreen"
  )
  Sys.sleep(0.1)
  dev.off()
  return(combined_results)
}
# plot_neutral_cluster_results()


# Question 21
state_initialise_adult <- function(num_stages,initial_size){
  sta_init <- rep(0,num_stages)
  sta_init[num_stages] <- initial_size
  return(sta_init)
}

# Question 22
state_initialise_spread <- function(num_stages,initial_size){
  ave_sta <- initial_size %% num_stages
  if(ave_sta == 0){
    sta_init <- rep(initial_size/num_stages,num_stages)
  }else{
    sta_init <- rep(floor(initial_size/num_stages),num_stages)
    for(i in 1:length(sta_init)){
      if(ave_sta == 0){
        break
      }
      sta_init[i] <- sta_init[i] + 1
      ave_sta <- ave_sta - 1
    }
  }
  return(sta_init)
}
# state_initialise_spread(num_stages=3, initial_size=8) 
# Question 23
deterministic_step <- function(state,projection_matrix){
  new_state <- projection_matrix %*% state
  return(new_state)
}

# Question 24
deterministic_simulation <- function(initial_state,projection_matrix,simulation_length){
  population_size <- c()
  for(i in 1: (simulation_length + 1)){
    if(i == 1){
      sta <- initial_state
      population_size <- c(population_size, sum(sta))
    }else{
      sta <- deterministic_step(sta,projection_matrix)
      population_size <- c(population_size, round(sum(sta)))
    }
  }
  return(population_size)
}

# Question 25
question_25 <- function(){
  
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
  proj_matrix <- growth_matrix + reproduction_matrix
  pop_adults <- deterministic_simulation(initial_state=c(0, 0, 0, 100),
                           projection_matrix=proj_matrix,
                           simulation_length=24)
  pop_ave <- deterministic_simulation(initial_state=c(25, 25, 25, 25),
                           projection_matrix=proj_matrix,
                           simulation_length=24)
  time_seires <- seq(0,24,1)
  # plot your graph here
  
  png(filename="question_25.png", width = 600, height = 400)
  g <- ggplot() +
    geom_line(aes(x=time_seires, y=pop_adults), colour = "black", size = 0.7) +
    geom_line(aes(x=time_seires, y=pop_ave), colour = "red", size = 0.7) +
    labs(x="Time", y="Population size") + 
    theme_bw()
  print(g)
  Sys.sleep(0.1)
  dev.off()
  
  return("The population size of both is increasing. The population size grows faster when the entire population is adults in the initial state.")
}
# question_25()

# Question 26
multinomial <- function(pool,probs) {
  if(probs[length(probs)-1] == 0 && probs[length(probs)] == 1){
    ass_pop <- c(rep(0, length(probs)-1), pool)
  }else if(probs[length(probs)-1] == 0 && probs[length(probs)] < 1 && probs[length(probs)] > 0){
    last_s <- rbinom(1, pool, probs[length(probs)])
    ass_pop <- c(rep(0, length(probs)-1), last_s)
  }else{
    ass_pop <- rmultinom(1, pool, probs)[,1]
  }
  return(ass_pop)
}
# Question 27
survival_maturation <- function(state, growth_matrix) {
  new_state <- rep(0,length(state))
  stage_num <- length(state)
  for(i in 1:stage_num) {
    pop <- state[i]
    probs <- growth_matrix[, i]
    ass_pop <- multinomial(pop, probs)
    new_state <- new_state + ass_pop
  }
  return(new_state)
}
# survival_maturation(state = c(0,0,0,0), growth_matrix <- matrix(c(0.1, 0.0, 0.0, 2.6,
#                                                                   0.5, 0.4, 0.0, 0.0,
#                                                                   0.0, 0.4, 0.7, 0.0,
#                                                                   0.0, 0.0, 0.25, 0.4),
#                                                                 nrow=4, ncol=4, byrow=T))
# 
# survival_maturation(state = c(4,5,4,6), growth_matrix <- matrix(c(0.1, 0.0, 0.0, 2.6,
#                                                                   0.5, 0.4, 0.0, 0.0,
#                                                                   0.0, 0.4, 0.7, 0.0,
#                                                                   0.0, 0.0, 0.25, 0.4),
#                                                                 nrow=4, ncol=4, byrow=T))
# 
# survival_maturation(state = c(4,5,4,6), growth_matrix <- matrix(c(1, 0, 0, 0,
#                                                                   0, 1, 0, 0,
#                                                                   0, 0, 1, 0,
#                                                                   0, 0, 0, 1),
#                                                                 nrow=4, ncol=4, byrow=T))
# 
# survival_maturation(state = c(4,5,4,6), growth_matrix <- matrix(c(0.5, 0.0, 0.0, 0.0,
#                                                                   0.5, 0.4, 0.0, 0.0,
#                                                                   0.0, 0.6, 0.7, 0.0,
#                                                                   0.0, 0.0, 0.3, 1.0),
#                                                                 nrow=4, ncol=4, byrow=T))
# 

# Question 28
random_draw <- function(probability_distribution) {
  return(sample(1: length(probability_distribution), 1, prob = probability_distribution))
}

# Question 29
stochastic_recruitment <- function(reproduction_matrix,clutch_distribution){
  re <- reproduction_matrix[1, ncol(reproduction_matrix)]
  mean_s <- sum(clutch_distribution * seq_along(clutch_distribution))
  re_prob <- re / mean_s 
  if(re_prob > 1){
    warning("Wrong!")
  }else{
    return(re_prob)
  }
}
# stochastic_recruitment(reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
#                                                        0.0, 0.0, 0.0, 0.0,
#                                                        0.0, 0.0, 0.0, 0.0,
#                                                        0.0, 0.0, 0.0, 0.0),
#                                                        nrow=4, ncol=4, byrow=T), clutch_distribution = c(0.1, 0.2, 0.1, 0.6))
# Question 30
offspring_calc <- function(state,clutch_distribution,recruitment_probability){
  total_offspring <- 0
  adults_num <- state[length(state)]
  num_clu <- rbinom(1, adults_num, recruitment_probability)
  if(num_clu == 0){
    return(total_offspring)
  }else{
    for(i in 1:num_clu){
      total_offspring <- total_offspring + random_draw(clutch_distribution)
    }
    return(total_offspring)
  }
}

# Question 31
stochastic_step <- function(state,growth_matrix,reproduction_matrix,clutch_distribution,recruitment_probability){
  new_state <- survival_maturation(state, growth_matrix)
  total_offspring <- offspring_calc(new_state,clutch_distribution,recruitment_probability)
  new_state[1] <- new_state[1] + total_offspring
  return(new_state)
}
# stochastic_step(state = c(2,3,3,4), growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
#                                                               0.5, 0.4, 0.0, 0.0,
#                                                               0.0, 0.4, 0.7, 0.0,
#                                                               0.0, 0.0, 0.25, 0.4),
#                                                             nrow=4, ncol=4, byrow=T), 
#                                     reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
#                                                                     0.0, 0.0, 0.0, 0.0,
#                                                                     0.0, 0.0, 0.0, 0.0,
#                                                                     0.0, 0.0, 0.0, 0.0),
#                                                                     nrow=4, ncol=4, byrow=T), 
#                                     clutch_distribution = c(0.3, 0.7, 0.4, 0.6), 
#                                     recruitment_probability = 0.5)
# 
# Question 32
stochastic_simulation <- function(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length){
  re_prob <- stochastic_recruitment(reproduction_matrix,clutch_distribution)
  population_size <- c(sum(initial_state), rep(0, simulation_length))
  for(i in 1:simulation_length){
    state <- stochastic_step(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,re_prob)
    if(sum(state) == 0){
      break
    }else{
      population_size[i+1] <- sum(state)
    }
  }
  return(population_size)
}
# stochastic_simulation(initial_state = c(0,0,0,100), growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
#                                                               0.5, 0.4, 0.0, 0.0,
#                                                               0.0, 0.4, 0.7, 0.0,
#                                                               0.0, 0.0, 0.25, 0.4),
#                                                             nrow=4, ncol=4, byrow=T),
#                                                   reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
#                                                                                   0.0, 0.0, 0.0, 0.0,
#                                                                                   0.0, 0.0, 0.0, 0.0,
#                                                                                   0.0, 0.0, 0.0, 0.0),
#                                                                                   nrow=4, ncol=4, byrow=T),
#                                                   clutch_distribution = c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03),
#                                                   simulation_length = 150)
# 


# Question 33
question_33 <- function(){
  clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
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
  adults <- state_initialise_adult(4,100)
  ave <- state_initialise_spread(4,100)
  simulation_length <- 24
  pop_s_ad <- stochastic_simulation(initial_state = adults, growth_matrix = growth_matrix, reproduction_matrix = reproduction_matrix, clutch_distribution = clutch_distribution, simulation_length = simulation_length)
  pop_s_ave <- stochastic_simulation(initial_state = ave, growth_matrix = growth_matrix, reproduction_matrix = reproduction_matrix, clutch_distribution = clutch_distribution, simulation_length = simulation_length)
  time <- seq(0,24,1)
  png(filename="question_33.png", width = 600, height = 400)
  g <- ggplot()+
    geom_line(aes(x=time,y=pop_s_ad),colour="red")+
    geom_line(aes(x=time,y=pop_s_ave),colour="black")+
    labs(x="Time",y="Population size")+
    theme_bw()
  print(g)
  Sys.sleep(0.1)
  dev.off()
  
  return("There is a significant difference between the two. Stochastic simulation does not produce smooth curves. This is because different probabilities are introduced when calculating the population.")
}
# question_33()
# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  adults_100_init <- 0
  ave_100_init <- 0
  adults_10_init <- 0
  ave_10_init <- 0
  extinct <- c()
  for(i in 1:100){
    file_name <- paste("pz123_2_",i,".rda",sep="")
    if (file.exists(paste("pz123_2_",i,".rda",sep=""))) {
      load(file_name)
      if(pop_size[length(pop_size)] == 0){
        if(i > 0 && i <=25){
          adults_100_init <- adults_100_init + 1
        }
        else if(i > 25 && i <=50){
          adults_10_init <- adults_10_init + 1
        }
        else if(i > 50 && i <=75){
          ave_100_init <- ave_100_init + 1
        }
        else if(i > 75 && i <=100){
          ave_10_init <- ave_10_init + 1
        }
      }
      print(file_name)
    } 
    else {
      next
    }    

  }
  prop_ad_100 <- adults_100_init/25
  prop_ad_10 <- adults_10_init/25
  prop_ave_100 <- ave_100_init/25
  prop_ave_10 <- ave_10_init/25
    
  gp_prop <- c(prop_ad_100, prop_ad_10, prop_ave_100, prop_ave_10)
  x_name <- c("Adult;Large;Pop", "Adult;Small;Pop", "Spread;Large;Pop", "Spread;Small;Pop")
  df <- data.frame(x_name, gp_prop)
  png(filename="question_36.png", width = 600, height = 400)
  # plot your graph here
  g <- ggplot(df, aes(x = x_name, y = gp_prop, fill = x_name))+
    geom_bar(stat = "identity", width = 0.4)+
    labs(x="Initial Population",y="Proportion of Extinction")+
    theme_bw()
    
  print(g)
  Sys.sleep(0.1)
  dev.off()
  
  return("Visualization shows that smaller populations are most likely to go extinct. This is because a smaller initial population, with a smaller base, leads to a higher probability of extinction.")
}
# question_36()
# Question 37
question_37 <- function(){
  df_l <- data.frame()
  df_s <- data.frame()
  for (i in 51:100) {
    if (file.exists(paste("pz123_2_",i,".rda",sep=""))) {
      load(paste("pz123_2_", i, ".rda", sep = ""))
      if (i > 50 && i <= 75) {
        df_l <- rbind(df_l, pop_size)
      }
      else if (i > 75 && i <= 100) {
        df_s <- rbind(df_s, pop_size)
      }
    }
    else{
      next
    }
  }
  mean_l <- colMeans(df_l)
  mean_s <- colMeans(df_s)
  ave_large <- state_initialise_spread(4,100)
  ave_small <- state_initialise_spread(4,10)

  projection_matrix <- matrix(c(0.1, 0.0, 0.0, 2.6,
                                0.5, 0.4, 0.0, 0.0,
                                0.0, 0.4, 0.7, 0.0,
                                0.0, 0.0, 0.25, 0.4),
                              nrow=4, ncol=4, byrow=T)
  simulation_length <- 150
  pop_s_de_large <- deterministic_simulation(initial_state = ave_large, projection_matrix = projection_matrix, simulation_length = simulation_length)
  pop_s_de_small <- deterministic_simulation(initial_state = ave_small, projection_matrix = projection_matrix, simulation_length = simulation_length)
  png(filename="question_37_small.png", width = 600, height = 400)
  # plot your graph for the small initial population size here
  g <- ggplot()+
    geom_line(aes(x=seq(0,150,1),y=pop_s_de_small),colour="red")+
    geom_line(aes(x=seq(0,150,1),y=mean_s),colour="black")+
    labs(x="Time",y="Population size with large initial Pop")+
    ggtitle("The Large Starting Population of Stochastic Simulation & Deterministic Simulation") +
    theme_bw()
  print(g)
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large.png", width = 600, height = 400)
  # plot your graph for the large initial population size here
  g2 <- ggplot()+
    geom_line(aes(x=seq(0,150,1),y=pop_s_de_large),colour="red")+
    geom_line(aes(x=seq(0,150,1),y=mean_l),colour="black")+
    labs(x="Time",y="Population size with small initial Pop")+
    ggtitle("The Small Starting Population of Stochastic Simulation & Deterministic Simulation") +
    theme_bw()
  print(g2)
  Sys.sleep(0.1)
  dev.off()
  
  return("For initial conditions with a larger number of samples, it is more appropriate to approximate the 'average' behavior of this stochastic system with a deterministic model. This is because populations with a smaller initial scale have a smaller base, and the stochastic simulation involves elements of chance.")
}
# question_37()


# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  
  
  png(filename="Challenge_A_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question B
Challenge_B <- function() {
  
  
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  
  
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function(){
  
  
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function(){
  
  
  
  png(filename="Challenge_F", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}
