### 0.1 library
  library(dplyr); library(magrittr); library(ggplot2); library(gridExtra); require(tidyr)
  source("F00.00.generic.R")
  source("F01.01.base.R")
  source("F01.02.models.R")
  
### 1.0 simulation parameters  
  # dataset-wise parameters
  # n = 120; nD <- nT <- nH <- 40; 
  n.sample <- c(20, 20, 20, 20) # sample size for H1, D1, H2, D2
  n.species = 1e+5
  
  # simulation-wise parameters
  n.sim = 10000
  method.stat = tester.set.HD.batch(n.sim=5, skeleton=TRUE)[[1]] %>% rownames
  # LB.nonz LB.zero LB.glob LN MAST.nonz MAST.zero MAST.glob KW Wg.nonz Wg.zero Wg.glob (Reserved)
  method = gsub("\\..*$","",method.stat) %>% unique
  # LB LN MAST KW Wagner (spare)
  
### 2.0 distribution parameters
# parameter1 = basic scenarios
  expand.grid(m=c(.5, 1, 2), t=c(1, 3, 10), p=c(.5,.9,.95)) %>% # normal scenarios 1-27
    rbind(c(5,3,.9)) %>%  # extreme scenario 1
    rbind(c(1,30,.9)) %>%   # extreme scenario 2
    rbind(c(1,3,.99)) %>%  # extreme scenario 3
    data.frame() %>%
    mutate(no = 1:n()) %>%       # add scenario numbers (no)
    dplyr::select(no, everything())  -> # reorder columns
    parameter1

# multipliers
  
  # delta = differential expression
  matrix(c(0, 0, 0,
           4, 0, 0,   0, 1, 0,    0, 0, 1,
           4, 1, 0,   4, 0, 1,    0, 1, 1), byrow = TRUE,
         nrow = 7, dimnames=list(1:7, c("m", "t", "p"))) %>% data.frame -> delta
  delta$detail = paste0("Effect_", c("null", "mu", "theta", "pi", "mu.theta", "mu.pi", "theta.pi"))
  
  matrix(c(0, 0, 0,   .5, 1, .5,   1, 4, 1), byrow = TRUE,
         nrow = 3, dimnames=list(1:3, c("m", "t", "p"))) %>% data.frame -> kappa
  kappa$detail = paste(c("no", "small", "large"), "batch effect")
  
  