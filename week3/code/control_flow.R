a <- TRUE
if (a == TRUE) {
  print ("a is TRUE")
} else {
  print ("a is FALSE")
}

z <- runif(1)
z

for (i in 1:10) {
  j <- i * i
  print(paste(i, " squared is", j))
}

for (species in c('Heliodoxa rubinoides',
                  'Boissonneaua jardini',
                  'Sula nebouxii')) {
  print(paste('The species is', species))
}

v1 <- c("a","bc","def")
for (i in v1) {
  print(i)
}

i <- 0
while (i < 10) {
  i <- i+1
  print(i^2)
}

# Checks if an integer is even
is.even <- function(n = 2) {
  if (n %% 2 == 0) {
    return(paste(n,'is even!'))
  } else {
    return(paste(n,'is odd!'))
  }
}

is.even(6)

# Checks if a number is a power of 2
is.power2 <- function(n = 2) {
  if (log2(n) %% 1==0) {
    return(paste(n, 'is a power of 2!'))
  } else {
    return(paste(n,'is not a power of 2!'))
  }
}

is.power2(4)

# Checks if a number is a power of 2
is.power2 <- function(n = 2) {
  if (log2(n) %% 1==0) {
    return(paste(n, 'is a power of 2!'))
  } else {
    return(paste(n,'is not a power of 2!'))
  }
}

is.power2(123444)

# Checks if a number is prime
is.prime <- function(n) {
  if (n==0) {
    return(paste(n,'is a zero!'))
  } else if (n==1) {
    return(paste(n,'is just a unit!'))
  }
  
  ints <- 2:(n-1)
  
  if (all(n%%ints!=0)) {
    return(paste(n,'is a prime!'))
  } else {
    return(paste(n,'is a composite!'))
  }
}

is.prime(97)
