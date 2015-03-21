# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted = matrix(nrow = n.doctors, ncol = n.days)
  has_adopted[, 1] = initial.doctors
  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
  for(a in 1:(n.days-1)){
    accept = sample(1000, size = 1)
    pickdoctor = sample(n.doctors, size = 2)
    select = has_adopted[pickdoctor, a]
    has_adopted[, a+1] = has_adopted[, a]
    if((sum(select) == 1) && (accept <= p*1000)){
      has_adopted[pickdoctor, a+1] = 1
    }
  }
  # return the output
  return(has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
initial.doctors = sample(c(0, 1), size = 20, replace=TRUE, prob=c(0.9, 0.1))

# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
a = sim.doctors(initial.doctors, length(initial.doctors), 15, .1)
b = sim.doctors(initial.doctors, length(initial.doctors), 15, .3)
c = sim.doctors(initial.doctors, length(initial.doctors), 15, .5)
d = sim.doctors(initial.doctors, length(initial.doctors), 15, .7)
e = sim.doctors(initial.doctors, length(initial.doctors), 15, .9)

apoints = vector(mode = "integer", length = 15)
for(i in 1:15) {
  apoints[i] = sum(a[,i])}

bpoints = vector(mode = "integer", length = 15)
for(i in 1:15){
  bpoints[i] = sum(b[,i])}

cpoints = vector(mode = "integer", length = 15)
for(i in 1:15){
  cpoints[i] = sum(c[,i])}

dpoints = vector(mode = "integer", length = 15)
for(i in 1:15){
  dpoints[i] = sum(d[,i])}

epoints = vector(mode = "integer", length = 15)
for(i in 1:15){
  epoints[i] = sum(e[,i])}

plot(x = 1:15, y = epoints, type="b", col="blue", main="Drug adoption", 
     xlab="Day", ylab="Number of doctors who adopted drug") # p = .9
lines(x = 1:15, y = bpoints, type="b", col="orange") # p = .3
lines(x = 1:15, y = cpoints, type="b", col="green") # p = .5
lines(x = 1:15, y = dpoints, type="b", col="black") # p = .7
lines(x = 1:15, y = apoints, type="b", col = "purple") # p = .1

