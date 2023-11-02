doit <- function(x) {
  temp_x <- sample(x, replace = TRUE)
  if (length(unique(temp_x)) > 30) {
    #only take mean if sample was sufficient
    print(paste("Mean of this sample was:", as.character(mean(temp_x))))
  }
  else {
    stop("Couldn't calculate mean: too few unique values!")
  }
}

set.seed(1345) # again, to get the same result for illustration

popn <- rnorm(50)

hist(popn)

lapply(1:15, function(i)
  doit(popn))
result <- lapply(1:15, function(i) try(doit(popn), TRUE))
class(result)
result
result <- vector("list", 15) #Preallocate/Initialize
for (i in 1:15)
{
  result[[i]] <- try(doit(popn), FALSE)
}

###### tryCatch ############

# for(i in 1:15) {
#   result[[i]] <- tryCatch({
#     doit(popn)
#   }, error = function(e) {
#     cat("Couldn't calculate mean: too few unique values!", conditionMessage(), "\n")
#     return(NA)
#   }, warring = function(w) {
#     print("!!!")
#     return(NA)
#   })
# }
