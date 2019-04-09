### 0. library
if(TRUE){
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra)
source("Script/F00.00.generic.R")
source("Script/F01.01.base.R")
source("Script/F02.01.simulation.R")
source("Script/F01.02.models-base.R")
source("Script/F01.02.models.R")
# devtools::install_github("RGLab/MAST");
library(MAST)
library(coin)

### 1. Create Simulation data
source("Script/C01.02.simulation.setup.R")
(parameter = parameter3); 
(delta = delta1); 
(kappa = kappa1)
n.sim; n.sample; 
n.genes=1e+4
print(test.dim <- method.stat %>% length)
}

if(TRUE){
  i = 1; j = 3; k = 29; 
  set.seed(i*10^2 + j*10 + k, kind = "Mersenne-Twister", normal.kind = "Inversion")
  data = rZINB.sim(n.sample = n.sample, n.genes=n.genes, scenario.delta = i, scenario.kappa = j, scenario.base = k,
                   baseParam = parameter,
                   delta.table = delta, 
                   kappa.table = kappa)
  data %>% dplyr::select(-phenotype, - batch) %>% apply(1, sum) -> data$sampleSum
  data %<>% dplyr::filter(sampleSum > 0)
}

### 2. Check LB method
l = 3305;
data.l = data.frame(y=data[,l], data[,c("phenotype", "batch", "sampleSum")])
test = FALSE; NAwarning = T
if (sum(data.l$y)==0) {
  data.frame(coef = rep(NA,3), pval = NA)
} else {
  ## print(data[,l]) (For debug only)
  tmp <- LB.test(data.l, sig = sig, test = T, NAwarning = T)       #logistic beta with betareg
  #tmp2 <- LB.old(data.l, sig = sig, test = T)  #logistic beta with gamlss
}


### 1~3. LB
LB.test <- function (data.l, sig = 0.05, test = FALSE, NAwarning = T) {
  out = matrix(NA, 3, 2, 
               dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval")))
  data.l$y.prop = data.l$y / data.l$sampleSum
  data.l$y.bin = ifelse(data.l$y > 0, 1, 0)
  data.l.positive = data.l %>% dplyr::filter(y > 0)
  
  # 1. beta model
  ctn.b = TRUE  # Checker to aquire data from model
  if(length(data.l.positive$y)<2){
    out1 = c(NA, NA)
    ctn.b = F
  }else if(length(unique(data.l.positive$batch)) <2){ # if there is only one batch
    
    bereg.pos = try(betareg(y.prop ~ phenotype, 
                            data = data.l.positive))
    
    if (any(class(bereg.pos) %in% "try-error")) {
      out1 = c(NA, NA)
      print(paste0("Betareg model Error(no batch)"))
      ctn.b = F
    }
    
    #print(summary(bereg.pos))
    if(ctn.b & NAwarning){
      out1 = summary(bereg.pos)$coefficients$mean[2,c(1,4)]
    }
  }else{
    bereg.pos = try(betareg(y.prop ~ phenotype + batch, 
                            data = data.l.positive))
    
    if (any(class(bereg.pos) %in% "try-error")) {
      out1 = c(NA, NA)
      print(paste0("Betareg model Error"))
      ctn.b = F
    }
    
    #print(summary(bereg.pos))
    if(ctn.b & NAwarning){
      out1 = summary(bereg.pos)$coefficients$mean[2,c(1,4)]
    }
  }
  
  # 2. logistic model
  ctn.l = TRUE  # Checker to aquire data from model
  bereg.bin = try(glm(y.bin ~ phenotype + batch, 
                      family = binomial, data = data.l))
  
  if (any(class(bereg.bin) %in% "try-error")) {
    print(paste0("Logistic model Error"))
    ctn.l = F
  }
  
  #print(summary(bereg.bin))
  if(ctn.l & NAwarning){
    out2 = summary(bereg.bin)$coefficients[2,c(1,4)]
  }
  # 3. null models
  ## 1. null beta
  ctn.nb = TRUE  # Checker to aquire data from model
  if(length(data.l.positive$y)<2){
    bereg.pos.null = NA
    ctn.nb = F
  }else if(length(unique(data.l.positive$batch)) <2){
    bereg.pos.null = try(betareg(y.prop ~ 1, 
                                 data = data.l.positive))
    if (any(class(bereg.pos.null) %in% "try-error")) {
      print(paste0("Betareg null model Error"))
      ctn.nb = FALSE
    }
  }else{
    bereg.pos.null = try(betareg(y.prop ~ batch, 
                                 data = data.l.positive))
    if (any(class(bereg.pos.null) %in% "try-error")) {
      print(paste0("Betareg null model Error"))
      ctn.nb = FALSE
    }
  }
  
  ## 2. null logistic
  ctn.nl = TRUE  # Checker to aquire data from model
  bereg.bin.null = try(glm(y.bin ~ batch, 
                           family = binomial, data = data.l))
  if (any(class(bereg.bin.null) %in% "try-error")) {
    print(paste0("Logistic null model Error"))
    ctn.nl = F
  }
  # 4. global likelihood
  if(!ctn.l | !ctn.nl){
    out3 = out1
  }else if(!ctn.b | !ctn.nb) {
    out3 = out2
  }else{
    chisq = 2*((logLik(bereg.bin)[1] + summary(bereg.pos)$loglik) - 
                 (logLik(bereg.bin.null)[1] + summary(bereg.pos.null)$loglik))
    
    df = ((- bereg.bin$df.residual) + 
            (- bereg.pos$df.residual))    -
      (( - bereg.bin.null$df.residual) + 
         (- bereg.pos.null$df.residual))
    
    out3 = matrix(c(chisq, 1 - pchisq(chisq, df)), 1, 2)
  }
  # 5. stack up
  out = rbind(out1, out2, out3)
  rownames(out) = c("LB.nonz", "LB.zero", "LB.glob")
  colnames(out) = c("Estimate", "pval")
  if(test == TRUE){
    print(out)
  }
  return(out)
}
