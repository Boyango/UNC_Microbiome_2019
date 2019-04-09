### 0.1 library
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra)
source("Script/F00.00.generic.R")
source("Script/F01.01.base.R")
source("Script/F02.01.simulation.R")
source("Script/F01.02.models-base.R")
source("Script/F01.02.models.R")
# devtools::install_github("RGLab/MAST");
library(MAST)
library(coin)

# required parameters from...
source("Script/C01.02.simulation.setup.R")
#parameter1; delta; kappa
(parameter = parameter3); 
(delta = delta1); 
(kappa = kappa1)

n.sim; n.sample; 
n.genes=1e+4
print(test.dim <- method.stat %>% length)

#
args = commandArgs(trailingOnly=TRUE)  # passed from script
cat("The Command Arg is: ", str(args))
i = args[1] %>% as.numeric  # 1..10
j = args[2] %>% as.numeric  # 1..5
k = args[3] %>% as.numeric  # 1..46
#i = 9; j = 5; k = 49


# k2. do testing for each sim. replicate
cat("i: ", i,", j: ",j,", k: ",k,", \n")
set.seed(i*10^2 + j*10 + k, kind = "Mersenne-Twister", normal.kind = "Inversion")
# 1. parameter

## param.set = param (i, j, k) # list of H1, D1, H2, D2 (status-batch)
param.set = param (i, j, k, baseParam = parameter, delta.table = delta, kappa.table = kappa)

# 2. data
data = rZINB.sim(n.sample = n.sample, n.genes=n.genes, scenario.delta = i, scenario.kappa = j, scenario.base = k,
                 baseParam = parameter,
                 delta.table = delta, 
                 kappa.table = kappa)
data %>% dplyr::select(-phenotype, - batch) %>% apply(1, sum) -> data$sampleSum
data %<>% dplyr::filter(sampleSum > 0)
cat("sample size is ", dim(data)[1], "out of ", sum(n.sample), ".\n")
 
#if (any(class(try(readRDS(paste0("output/R0201sim181201/result.", i, ".", j, ".", k,".rds")))) %in% "try-error")) {

# do the tests on the ramdon ZINB distribution we created
tmp <- tester.set.HD.batch(data, n.sim=n.sim)

## To save the result
# Check and create the folder
save_path = paste0("Output/R0201sim", Sys.Date(), "/")
save_file = paste0("Output/R0201sim", Sys.Date(), "/result.", i, ".", j, ".", k,".rds")

if (file.exists(save_path)) {
  cat("\n subDir exists in mainDir and is a directory")
} else if (file.exists(save_file)) {
  cat("\n subDir exists in mainDir but is a file")
  # you will probably want to handle this separately
} else {
  cat("\n subDir does not exist in mainDir - creating")
  dir.create(file.path(save_path))
}

# Save the result
saveRDS(tmp, save_file)

# R0201sim180718 #n.sim=10000,  R0201sim180711 #n.sim=1000
#}

