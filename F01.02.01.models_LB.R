### 1~3. LB
LB2 <- function (data, sig = 0.05) {
  if(!require(betareg)){
    install.packages("betareg")
  }
  data$y.prop = data$y / data$sampleSum
  data$y.bin = ifelse(data$y > 0, 1, 0)
  data.positive = data %>% dplyr::filter(y > 0)
  
  # 1. beta model
  bereg.pos = try(betareg(y.prop ~ phenotype + batch, 
                          data = data.positive))
  
  if (any(class(bereg.pos) %in% "try-error")) {
    return(matrix(NA, 3, 2, 
                  dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval"))))}
  
  #print(summary(bereg.pos))
  out1 = summary(bereg.pos)$coefficients$mean[2,c(1,4)]
  
  # 2. binomial model
  bereg.bin = try(glm(y.bin ~ phenotype + batch, 
                     family = binomial, data = data))
  
  if (any(class(bereg.bin) %in% "try-error")) {
    return(matrix(NA, 3, 2, 
                  dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval"))))}
  
  #print(summary(bereg.bin))
  out2 = summary(bereg.bin)$coefficients[2,c(1,4)]
  
  # 3. null models
  ## 1. null beta
  bereg.pos.null = try(betareg(y.prop ~ batch, 
                          data = data.positive))
  if (any(class(bereg.pos.null) %in% "try-error")) {
    return(matrix(NA, 3, 2, 
                  dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval"))))}

    ## 2. null binomial
  bereg.bin.null = try(glm(y.bin ~ batch, 
                      family = binomial, data = data))
  if (any(class(bereg.bin.null) %in% "try-error")) {
    return(matrix(NA, 3, 2, 
                  dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval"))))}
  
  # 4. global likelihood
  chisq = 2*((logLik(bereg.bin)[1] + summary(bereg.pos)$loglik) - 
            (logLik(bereg.bin.null)[1] + summary(bereg.pos.null)$loglik))
  
  df = ((- bereg.bin$df.residual) + 
       (- bereg.pos$df.residual))    -
                  (( - bereg.bin.null$df.residual) + 
                  (- bereg.pos.null$df.residual))
  
  out3 = matrix(c(chisq, 1 - pchisq(chisq, df)), 1, 2)
  
  # 5. stack up
  out = rbind(out1, out2, out3)
  rownames(out) = c("LB.nonz", "LB.zero", "LB.glob")
  colnames(out) = c("Estimate", "pval")
  print("*******************************************************")
  return(out)
}

if (FALSE) {
  data = rZINB.sim(n.sample=rep(3,4),n.genes=10, 1,1,1,baseParam = parameter,
                   delta.table = delta, 
                   kappa.table = kappa)
  data = data %>% mutate(y = y.1)
  LB(data)
  LB2(data)
  
}

## error catch
## likelihood summation
## first two rows of out