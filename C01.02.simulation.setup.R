### 0.1 library
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra); require(tidyr)
source("Script/F00.00.generic.R")
source("Script/F01.01.base.R")
source("Script/F01.02.models.R")

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

expand.grid(m=c(2, 3, 5, 10), t=c(0.5, 1), p=c(.3, .5, .6, .9, .95)) %>% # normal scenarios 1-40
  rbind(expand.grid(m=c(2, 3, 5), t=c(5), p=c(.9, .95))) %>%  # extreme scenario 41-46
  data.frame() %>%
  mutate(no = 1:n()) %>%       # add scenario numbers (no)
  dplyr::select(no, everything())  -> # reorder columns
  parameter1


expand.grid(m=c(2, 3, 5, 10), t=c(0.5, 1), p=c(.65, .7, .75, .8, .85 )) %>% # normal scenarios 
  data.frame() %>%
  mutate(no = 1:n()) %>%       # add scenario numbers (no)
  dplyr::select(no, everything())  -> # reorder columns
  parameter2

expand.grid(m=c(2, 3, 5, 10), t=c(0.5, 1), p=c(.3, .5, .6 ,.7 ,.8, .9, .95)) %>% # normal scenarios 1-40
  rbind(expand.grid(m=c(2, 3, 5), t=c(5), p=c(.9, .95))) %>%  # extreme scenario 41-46
  data.frame() %>%
  mutate(no = 1:n()) %>%       # add scenario numbers (no)
  dplyr::select(no, everything())  -> # reorder columns
  parameter3

# multipliers

# delta = differential expression
matrix(c(0, 0, 0,
         1, 0, 0,  0, 1,  0,    0, 0, -1,
         1, 1, 0,  1, 0, -1,    0, 1, -1,
         1, 0, 1,  1, -1, 0,    0, -1, -1), byrow = TRUE,
       nrow = 10, dimnames=list(1:10, c("m", "t", "p"))) %>% 
  data.frame ->
  delta1
delta1$detail = paste0("Effect_", c("null", "mu(D>H)", "theta(D>H)", "pi(D<H)", 
                                   "mu(D>H).theta(D>H)", "mu(D>H).pi(D<H)", "theta(D>H).pi(D<H)",
                                   "mu(D>H),pi(D>H)","mu(D>H).theta(D<H)","theta(D<H).pi(D<H)"))

matrix(c(0, 0, 0,   .5, -.5, -.5,   1, -1, -1,   .5, .5, -.5,   1, 1, -1), byrow = TRUE,
       nrow = 5, dimnames=list(1:5, c("m", "t", "p"))) %>% 
  data.frame -> 
  kappa1
kappa1$detail = paste(c("no","small(+,-,-)", "large(+,-,-)", "small(+,+,-)", "large(+,+,-)"), "batch effect")

