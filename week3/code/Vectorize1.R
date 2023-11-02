M <- matrix(runif(1000000),1000,1000)

SumAllElements <- function(M) {
  Dimensions <- dim(M)
  Tot <- 0
  for (i in 1:Dimensions[1]) {
    for (j in 1:Dimensions[2]) {
      Tot <- Tot + M[i,j]
    }
  }
  return (Tot)
}

print("Using loops, the time taken is:")
print(system.time(SumAllElements(M)))

print("Using the in-built vectorized function, the time taken is:")
print(system.time(sum(M)))

NoPreallocFun <- function(x) {
  a <- vector() # empty vector
  for (i in 1:x) {
    a <- c(a, i) # concatenate
    print(a)
    print(object.size(a))
  }
}

system.time(NoPreallocFun(10))

PreallocFun <- function(x) {
  a <- rep(NA, x) # pre-allocated vector
  for (i in 1:x) {
    a[i] <- i # assign
    print(a)
    print(object.size(a))
  }
}

system.time(PreallocFun(10))
