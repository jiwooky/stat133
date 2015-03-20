#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)
bml.init <- function(r, c, p){
carcount = round(r*c*p, 0)
density = c(rep(0, r*c-carcount), rep(1, carcount/2), rep(2, carcount/2))
m = matrix(data = sample(density, r*c, replace = TRUE), nrow = r, ncol = c)
  return(m)
}
hola = matrix(data = c(0, 2, 2, 2, 1, 0, 1, 1, 1, 0 ,1, 0, 0, 1, 0, 1), nrow = 4,ncol = 4)
#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
bml.north = function(m){
compare = m
for(i in 1:nrow(m)){
for(j in 1:ncol(m)){
  if((i == 1) & (compare[nrow(m), j] == 0) & (compare[i,j] == 1)){
    m[i, j] = 0
    m[nrow(m), j] = 1}
  if(i > 1){
    if((compare[i-1, j] == 0) & (compare[i, j] == 1)){
    m[i, j] = 0
    m[i-1, j] = 1}}
}
}
  return(m)
}

bml.east = function(m){
compare = m
  for(i in 1:nrow(m)){
  for(j in 1:ncol(m)){
      if((j == ncol(m)) & (compare[i, 1] == 0) & (compare[i, j] == 2)){
        m[i, j] = 0
        m[i, 1] = 2}
      if(j < ncol(m)){
        if((compare[i, j+1] == 0) & (compare[i, j] == 2)){
        m[i, j] = 0
        m[i, j+1] = 2}}
    }
  }
  return(m)
}

bml.step <- function(m){
  mold = m
  n = bml.north(m)
  e = bml.east(n)
  if(identical(mold, e)){
    grid.new = FALSE}
  else{
    grid.new = TRUE}
  return(list(e, grid.new))
}
x = bml.step(hola)
hola2 = x[[1]]
hola2see = x[[2]]
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
m = bml.init(r, c, p)
hold = bml.step(m)
m = hold[[1]]
i = 1
while((i < 10000) & (hold[[2]])){
    i = i + 1
    hold = bml.step(m)
    m = hold[[1]]}
return(list(m ,i))
}
q = bml.sim(20, 20, .4)
q1 = q[[1]]
