logit <- function(x){
  y = log(x / (1-x))
  return(y)
}

expit <- function(y){
  x = exp(y)/(exp(y)+1)
  return(x)
}