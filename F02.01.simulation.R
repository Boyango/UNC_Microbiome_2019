
param <- function(scenario.delta, scenario.kappa, scenario.base = 1:dim(baseParam)[1],
                  baseParam,
                  delta.table, kappa.table) {
  require(dplyr)
  if (length(scenario.delta) != 1 & length(scenario.kappa) != 1) stop("scenario.delta(pheno) and scenario.kappa(batch) should be of length 1.")
  # output: list of phenotype-batch combinations with a dataframe(base scenarios x params) as their elements
  m = length(scenario.base)
  
  # 1. getting base parameters and their log & logit values
  parameters = baseParam[scenario.base,-1, drop=FALSE] %>% as.matrix
  log.param = cbind(log(parameters[,1:2, drop=FALSE]),p=qlogis(parameters[,3, drop=FALSE]))

  # 2. getting effects
  effect1 = delta.table[scenario.delta,1:3] %>% as.numeric %>% matrix(nrow=m, ncol=3, byrow=TRUE)
  effect2 = kappa.table[scenario.kappa,1:3] %>% as.numeric %>% matrix(nrow=m, ncol=3, byrow=TRUE)


  # 3. effect-adjusted params
  list(H.1 = log.param - effect1/2 - effect2/2,  #healthy batch1
       D.1 = log.param + effect1/2 - effect2/2,  #diseased batch1
       H.2 = log.param - effect1/2 + effect2/2,  #healthy batch2
       D.2 = log.param + effect1/2 + effect2/2   #diseased batch2
       ) -> log.param
  
  # 4. back-transform to original scale
  parameters = lapply(log.param, function(s) cbind(exp(s[,1:2, drop=FALSE]),p=plogis(s[,3, drop=FALSE])))
  
  return(parameters)
  # parameters = lapply(1:4, function(s) parameters)
  #   names(parameters) = c("Healthy-Batch1", "Disease-Batch1", "Healthy-Batch2", "Disease-Batch2")
}

rZINB.sim <- function(n.sample = c(20, 20, 20, 20), n.genes=1,
                      scenario.delta, scenario.kappa, scenario.base,
                      baseParam,
                      delta.table, kappa.table) {
  
  require(dplyr); require(magrittr); require(tidyr)
  if (length(scenario.base)!=1) stop("length of scenario.base is not 1.")
  
  param.set = param (scenario.delta = scenario.delta, scenario.kappa = scenario.kappa, 
                     scenario.base = scenario.base,
                     baseParam = baseParam, delta.table = delta.table, 
                     kappa.table = kappa.table)
  dat = lapply(1:length(param.set), function(s) {
    data.frame(y = matrix(rZINB (n = n.sample[s]*n.genes, param=param.set[[s]] %>% as.numeric), nrow=n.sample[s], ncol=n.genes), 
               cat = names(param.set)[s])
    })
  dat = do.call(rbind, dat)
  dat %>% 
    separate(col = cat, into = c("phenotype", "batch"), sep="\\.") %>% 
    mutate(phenotype = factor(phenotype), batch = factor(batch)) -> dat
  dat$sampleSum = dplyr::select(dat, -phenotype, -batch) %>% apply(1, sum)
  return(dat)
}

if (FALSE) {#example
  param(2,3,1)
  param(2,3,1:3)
  param(2,3)[[1]]
  
  rZINB.sim(n.sample=rep(3,4),n.genes=1, 1,1,1)
  rZINB.sim(n.sample=rep(3,4),n.genes=10, 1,1,1)
}
 
