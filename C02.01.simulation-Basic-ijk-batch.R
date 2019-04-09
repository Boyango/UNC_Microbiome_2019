### 0.1 library
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra)
source("F00.00.generic.R")
source("F01.01.base.R")
source("F02.01.simulation.R")
source("F01.02.models-base.R")
source("F01.02.models.R")
# devtools::install_github("RGLab/MAST");
library(MAST)
library(coin)

# required parameters from...
source("C01.02.simulation.setup.R")
parameter1; delta; kappa
n.sim; n.sample; 
n.genes=1e+4
print(test.dim <- method.stat %>% length)

args = commandArgs(trailingOnly=TRUE)  # passed from script
i = args[1] %>% as.numeric  # 1..7
j = args[2] %>% as.numeric  # 1..3
k = args[3] %>% as.numeric  # 1..30

# k2. do testing for each sim. replicate
  cat("i: ", i,", j: ",j,", k: ",k,", \n")
  set.seed(i*10^2 + j*10 + k)
  # 1. parameter
  param.set = param (i, j, k) # list of H1, D1, H2, D2 (status-batch)
  # 2. data
  data = rZINB.sim(n.sample = n.sample, n.genes=n.genes, scenario.delta = i, scenario.kappa = j, scenario.base = k)
  data %>% dplyr::select(-phenotype, - batch) %>% apply(1, sum) -> data$sampleSum
  data %<>% dplyr::filter(sampleSum > 0)
cat("sample size is ", dim(data)[1], "out of ", sum(n.sample), ".\n")
  
  if (any(class(try(readRDS(paste0("output/R0201sim180718/result.", i, ".", j, ".", k,".rds")))) %in% "try-error")) {
    tmp <- tester.set.HD.batch(data, n.sim=n.sim)
    saveRDS(tmp, paste0("output/R0201sim180718/result.", i, ".", j, ".", k,".rds"))
    # R0201sim180718 #n.sim=10000,  R0201sim180711 #n.sim=1000
  }
  