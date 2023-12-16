# written by james rosindell james@rosindell.org Imperial college london released open source under an MIT license

# Assign random speciation rates to class
CMEE_2023 <- c(
  "mg2020",
  "pvt23",
  "fx23",
  "gk523",
  "tr220",
  "bd623",
  "hg2423",
  "jg923",
  "zh1323",
  "yj2323",
  "dl2423",
  "zml23",
  "ts920",
  "scs23",
  "sw920",
  "cw323",
  "xx123",
  "yy1423",
  "pz123",
  "lz723",
  "kc523",
  "zs519",
  "jjh13",
  "xw1722",
  "xy23"
  )




choose_student <- function(class) {
    print(sample(class,1))
}

choose_student_2 <- function(class,seedin = 1) {
    set.seed(seedin)
    print(sample(class,1))
}

choose_student_3 <- function(class,seedin=-1) {
    if (seedin <= 0){
        set.seed(floor(proc.time()[3]*1000))
    }
    else {
        set.seed(seedin)
    }
    print(sample(class,1))
}

assign_student_number <- function(class=CMEE_2023,seedin=2023,min=0.002,max=0.007,sigfig=4,unique=TRUE) {
    if (seedin <= 0){
        set.seed(floor(proc.time()[3]*1000))
    }
    else {
        set.seed(seedin)
    }
    speciation_values <- signif(runif(length(class))*(max-min)+min,sigfig)
    if (unique){
        while(length(unique(speciation_values)) < length(class)){
            speciation_values <- signif(runif(length(class))*(max-min)+min,sigfig)
        }
    }
    return(cbind(speciation_values,class))
}

print(assign_student_number())

