n.test = 12 #"LB.nonz", "LB.zero", "LB.glob", "LN", "MAST.nonz", "MAST.zero", "MAST.glob", "KW", "Wg.nonz", "Wg.zero", "Wg.glob", "Reserved"
if(!require(betareg)){
  install.packages("betareg")
}


### 0. testing a set of methods at once
# testing all methods using counts (or RPK) of a single gene (old: single gene. cannot do MAST).
# HD: binary phenotype (healthy-diseased)
tester.set.HD.batch <- function(data, n.sim = 1000, sig = 0.05, skeleton = FALSE) {
  # description
  # data should have y and sampleSum    all n.sample x (n.sim(gene) + 3 (phenotype + batch + sampleSum))
  #          outcome (phenotype), nuisance (batch)
  # skeleton: returning only skeleton (for simulation structure)
  require(magrittr)
  
  # 0.0 skeleton #empty matrix
  result = list(coef = matrix(NA, n.test, n.sim), # n.test=12, n.sim=1000
                pval = matrix(NA, n.test, n.sim))
  result %<>% lapply(function(x) {
    rownames(x) <- c("LB.nonz", "LB.zero", "LB.glob", "LN", "MAST.nonz", "MAST.zero", "MAST.glob", "KW", "Wg.nonz", "Wg.zero", "Wg.glob", "(Reserved)")
    x})
  if (skeleton) {return(result)}
  
  # 0.1 data
  gsub("y\\.","",names(data)) %>% as.numeric %>% na.omit %>% as.numeric -> genes
  if (n.sim > genes[length(genes)]) stop(paste0("Only ", length(genes), " genes provided, while trying to do ", n.sim, " simulations."))
  genes = genes[1:min(n.sim,length(genes))]
  
  data = data.frame(data[,1:length(genes)], phenotype = data$phenotype, batch = data$batch)
  ## print(head(data))    (No longer need to check data)
  if (!"sampleSum" %in% names(data)) {
    data %>% dplyr::select(-phenotype, - batch) %>% apply(1, sum) -> data$sampleSum
  }
  
  
  cat("1-3. Logistic Beta\n")
  #1-3. LB
  for (l in genes) {
    cat (" l = ",l," ")
    # if (l %% 200 == 0) {print("l = ")}
    data.l = data.frame(y=data[,l], data[,c("phenotype", "batch", "sampleSum")])
    if (sum(data.l$y)==0) {
      tmp <- data.frame(coef = rep(NA,3), pval = NA)
    } else {
      ## print(data[,l]) (For debug only)
      tmp <- LB.test(data.l, sig = sig, NAwarning = T)  #logistic beta
    }
    result[[1]][1:3, l] <- tmp[1:3,1] #coef. 1:3 corresponds to "LB.nonz", "LB.zero", "LB.glob"
    result[[2]][1:3, l] <- tmp[1:3,2] #pval. 1:3 corresponds to "LB.nonz", "LB.zero", "LB.glob"
  }
  
  #4. LN
  cat("\n4. Log normal\n l = ")
  for (l in genes) {
    cat (l," ")
    # if (l %% 200 == 0) {print("l = ")}
    data.l = data.frame(y=data[,l], data[,c("phenotype", "batch", "sampleSum")])
    if (sum(data.l$y)==0) {
      tmp <- data.frame(coef = NA, pval = NA)
    } else {
      tmp <- LN(data.l, sig = sig)  #log normal
    }
    result[[1]][4, l] <- tmp[1,1] #coef.
    result[[2]][4, l] <- tmp[1,2] #pval.
  }
  
  #5-7. MAST
  cat("\n5-7. MAST\n")
  # tmp <- MAST(data, sig = sig)  #MAST
  # tmp <- data.frame(coef = rep(NA,3), pval = NA)    #MAST maybe not applicable
  tmp <- MAST(data, sig = sig)
  result[[1]][5:7, ] <- tmp[[1]][1:3,] #coef. 1:3 corresponds to "MA.nonz", "MA.zero", "MA.glob"
  result[[2]][5:7, ] <- tmp[[2]][1:3,] #pval. 1:3 corresponds to "MA.nonz", "MA.zero", "MA.glob"
  
  
  #8. KW
  cat("\n8. Kruskal Wallis\n l = ")
  for (l in genes) {
    cat (l," ")
    # if (l %% 200 == 0) {print("l = ")}
    data.l = data.frame(y=data[,l], data[,c("phenotype", "batch", "sampleSum")])
    if (sum(data.l$y)==0) {
      tmp <- data.frame(coef = NA, pval = NA)
    } else {
      tmp <- KW(data.l, sig = sig)  #KW
    }
    result[[1]][8, l] <- tmp[1,1] #coef.
    result[[2]][8, l] <- tmp[1,2] #pval.
  }
  
  #9-11. Wagner
  cat("\n9-11. Wagner (2-part)\n l = ")
  for (l in genes) {
    cat (l," ")
    # if (l %% 200 == 0) {print("l = ")}
    data.l = data.frame(y=data[,l], data[,c("phenotype", "batch", "sampleSum")])
    if (sum(data.l$y)==0) {
      tmp <- data.frame(coef = rep(NA,3), pval = NA)
    } else {
      tmp <- Wagner(data.l, sig = sig, zeroModel = "logistic")
    }
    result[[1]][9:11, l] <- tmp[1:3,1] #coef.
    result[[2]][9:11, l] <- tmp[1:3,2] #pval.
  }
  
  #12. (reserved)
  cat("12. Reserved\n")
  # tmp <- data.frame(coef = NA, pval = NA)    #reserved for possible addition
  # result[[1]][12,1] <- tmp[1,1]
  # result[[2]][12,1] <- tmp[1,2]
  
  return(result)
}

# testing all methods using counts (or RPK) of a single gene (old: single gene. cannot do MAST).
tester.set.HD.single <- function(data, sig = 0.05, skeleton = FALSE) {
  # description
  # data should have y and sampleSum    all n.sample x 1(gene)
  #          outcome (phenotype), nuisance (batch)
  require(magrittr)
  
  # 0.1 skeleton
  result = list(coef = matrix(NA, n.test, n.sim), # n.test=12, n.sim=1000
                pval = matrix(NA, n.test, n.sim))
  result %<>% lapply(function(x) {
    rownames(x) <- c("LB.nonz", "LB.zero", "LB.glob", "LN", "MAST.nonz", "MAST.zero", "MAST.glob", "KW", "Wg.nonz", "Wg.zero", "Wg.glob", "Reserved")
    x})
  if (skeleton) {return(result)}
  
  
  #1-3. LB
  tmp <- LB.test(data, sig = sig)  #logistic beta
  result[[1]][1:3, 1] <- tmp[1:3,1] #coef. 1:3 corresponds to "LB.nonz", "LB.zero", "LB.glob"
  result[[2]][1:3, 1] <- tmp[1:3,2] #pval. 1:3 corresponds to "LB.nonz", "LB.zero", "LB.glob"
  # print("1. LB done")
  # print(result)
  
  #4. LN
  tmp <- LN(data, sig = sig)  #log normal
  result[[1]][4,1] <- tmp[1,1]
  result[[2]][4,1] <- tmp[1,2]
  # print("4. LN done")  
  # print(result)
  
  #5-7. MAST
  # tmp <- MAST(data, sig = sig)  #MAST
  tmp <- data.frame(coef = rep(NA,3), pval = NA)    #MAST maybe not applicable
  result[[1]][5:7, 1] <- tmp[1:3,1] #coef. 1:3 corresponds to "MA.nonz", "MA.zero", "MA.glob"
  result[[2]][5:7, 1] <- tmp[1:3,2] #pval. 1:3 corresponds to "MA.nonz", "MA.zero", "MA.glob"
  # print("5. MAST done")
  
  #8. KW
  tmp <- KW(data, sig = sig)  #KW
  result[[1]][8,1] <- tmp[1,1]
  result[[2]][8,1] <- tmp[1,2]
  # print("8. KW done")   
  
  #9. Wagner
  tmp <- Wagner(data, sig = sig, zeroModel = "logistic")
  # tmp <- data.frame(coef = NA, pval = NA)
  result[[1]][9:11,1] <- tmp[,1]
  result[[2]][9:11,1] <- tmp[,2]
  # print("9. Wagner done")   
  
  #10. (reserved)
  tmp <- data.frame(coef = NA, pval = NA)    #reserved for possible addition
  result[[1]][12,1] <- tmp[1,1]
  result[[2]][12,1] <- tmp[1,2]
  
  return(result)
}

if (FALSE) {# examples
  data = rZINB.sim(n.sample=rep(3,4),n.genes=30, 1,1,1)
  data %>% dplyr::select(-phenotype, - batch) %>% apply(1, sum) -> data$sampleSum
  data.1 = data.frame(y=data[,1], data[,c("phenotype", "batch", "sampleSum")])
  a = tester.set.HD.batch(data, n.sim=30)
}


### 1~3. LB
LB.old <- function (data, sig = 0.05, test = FALSE) {
  require(gamlss)
  data$y.prop = data$y / data$sampleSum  #sampleSum[j] = sum_g y_g,j (g:gene, j:sample)
  # print(head(data$y.prop))  
  # 1. two-part models
  bereg = try(gamlss(y.prop ~ phenotype + batch, nu.formula = ~ phenotype + batch,
                     family = BEZI(sigma.link = "log"), data = data,
                     control = gamlss.control(n.cyc = 100, trace = FALSE)))
  
  if (any(class(bereg) %in% "try-error")) {
    return(matrix(NA, 3, 2, 
                  dimnames = list(c("LB.nonz", "LB.zero", "LB.glob"), c("Estimate", "pval"))))}
  
  out1 = summary(bereg)[c(2,6), c(1,4)]
  
  # Alternative code should be updated (b/c when vcov fails, qr should be used.)
  # bereg.mat = c(try(suppressWarnings(vcov(bereg, type = "all", 
  #                   robust = F, hessian.fun = "R")), silent = TRUE), 
  #               df.res = bereg$df.res)
  # coef <- bereg.mat$coef
  # pvalue <- 2 * pt(-abs(coef/bereg.mat$se), bereg.mat$df.res)
  # pheno.index = grep("phenotype", names(coef))  #location of phenotype
  # out1 = cbind(coef, pvalue)[pheno.index,]
  
  # 2. global test
  bereg.null = try(gamlss(y.prop ~ batch, nu.formula = ~ batch,
                          family = BEZI(sigma.link = "log"),  data = data,
                          control = gamlss.control(n.cyc = 100, trace = FALSE)))
  if (any(class(bereg.null) %in% "try-error")) {bereg.null <- list(G.deviance = NA, df.residual = NA)}
  
  chisq = bereg.null$G.deviance - bereg$G.deviance
  df = bereg.null$df.residual - bereg$df.residual
  out2 = matrix(c(chisq, 1 - pchisq(chisq, df)), 1, 2)
  
  # 3. stack up
  out = rbind(out1, out2)
  rownames(out) = c("LB.nonz", "LB.zero", "LB.glob")
  colnames(out) = c("Estimate", "pval")
  if (test == TRUE){
    print(out)
  }
  return(out)
}

### 1~3. LB
LB <- function (data, sig = 0.05, test = FALSE , NAwarning = T) {
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
  if(test == TRUE){
    print(out)
  }
  return(out)
}


if (FALSE) {
  data = rZINB.sim(n.sample=rep(3,4),n.genes=10, 1,1,1)
  
  LB.old(data %>% mutate(y=y.1))
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
print("line 314")
  }else if(length(unique(data.l.positive$batch)) <2){ # if there is only one batch
print("line 316")
    bereg.pos = try(betareg(y.prop ~ phenotype, 
                            data = data.l.positive))
print("line 319")    
    if (any(class(bereg.pos) %in% "try-error")) {
      out1 = c(NA, NA)
      print(paste0("Betareg model Error(no batch)"))
      ctn.b = F
print("line 324")
    }
    
    #print(summary(bereg.pos))
    if(ctn.b & NAwarning){
      out1 = summary(bereg.pos)$coefficients$mean[2,c(1,4)]
print("line 330")      
    }
  }else{
    bereg.pos = try(betareg(y.prop ~ phenotype + batch, 
                            data = data.l.positive))
print("line 335")    
    if (any(class(bereg.pos) %in% "try-error")) {
      out1 = c(NA, NA)
      print(paste0("Betareg model Error"))
      ctn.b = F
print("line 340")
    }
    
    #print(summary(bereg.pos))
    if(ctn.b & NAwarning){
      out1 = summary(bereg.pos)$coefficients$mean[2,c(1,4)]
print("line 340")
    }
  }
  
  # 2. logistic model
  ctn.l = TRUE  # Checker to aquire data from model
  bereg.bin = try(glm(y.bin ~ phenotype + batch, 
                      family = binomial, data = data.l))
print("line 354")  
  if (any(class(bereg.bin) %in% "try-error")) {
    print(paste0("Logistic model Error"))
    ctn.l = F
print("line 358")
  }
  
  #print(summary(bereg.bin))
  if(ctn.l & NAwarning){
    out2 = summary(bereg.bin)$coefficients[2,c(1,4)]
print("line 364")
  }
  # 3. null models
  ## 1. null beta
  ctn.nb = TRUE  # Checker to aquire data from model
  if(length(data.l.positive$y)<2){
    bereg.pos.null = NA
    ctn.nb = F
print("line 372")
  }else if(length(unique(data.l.positive$batch)) <2){
print("line 374")
    bereg.pos.null = try(betareg(y.prop ~ 1, 
                                 data = data.l.positive))
print("line 377")
    if (any(class(bereg.pos.null) %in% "try-error")) {
      print(paste0("Betareg null model Error"))
      ctn.nb = FALSE
print("line 381")
    }
  }else{
print("line 3784")
    bereg.pos.null = try(betareg(y.prop ~ batch, 
                                 data = data.l.positive))
print("line 387")
    if (any(class(bereg.pos.null) %in% "try-error")) {
      print(paste0("Betareg null model Error"))
      ctn.nb = FALSE
print("line 391")
    }
  }
  
  ## 2. null logistic
  ctn.nl = TRUE  # Checker to aquire data from model
print("line 397")
  bereg.bin.null = try(glm(y.bin ~ batch, 
                           family = binomial, data = data.l))
print("line 400")
  if (any(class(bereg.bin.null) %in% "try-error")) {
    print(paste0("Logistic null model Error"))
    ctn.nl = F
print("line 404")
  }
  # 4. global likelihood
  if(!ctn.l | !ctn.nl){
print("line 408")
    out3 = out1
  }else if(!ctn.b | !ctn.nb) {
print("line 411")
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


### 4. Log-normal
LN <- function (data, sig = 0.05, epsilon = 1) {
  # log-transformation
  data$log2y = log2(data$y + epsilon)
  
  # fitting a linear model
  out = lm(log2y ~ phenotype + batch, data = data)
  out = matrix(summary(out)$coef[2, c(1,4)], nrow = 1)
  colnames(out) = c("Estimate", "pval")
  
  return(out)
}

### 5-7. MAST TBD!!!
MAST <- function (data, sig = 0.05) {
  # devtools::install_github("RGLab/MAST")
  require (MAST)
  
  # whole-data-level test. not adequate for inidivdual-gene-level test.
  
  name = names(data)
  gene = which(grepl("y\\.", name))
  gene.name = gsub("y\\.", "", name[gene])
  
  # log-transformation
  data[,gene] = log2(data[,gene] + 1)
  cData = data %>% transmute(wellKey = 1:n(), phenotype, batch)
  data = t(as.matrix(data[,gene]))
  dimnames(data) = list(gene.name, cData$wellKey)
  
  sca <- FromMatrix(data, cData = cData, 
                    fData = data.frame(primerid = gene.name))
  
  # Recalculating the cellular detection rate (ngeneson)
  cdr2 <-colSums(assay(sca) > 0)
  colData(sca)$cngeneson <- scale(cdr2)
  
  zlm.out <- try(zlm( ~ phenotype + batch + cngeneson, sca))
  
  if (any(class(zlm.out) %in% "try-error")) {
    result = matrix(NA, length(gene.name), 3, 
                    dimnames = list(gene.name, c("cont", "disc", "hurdle")))
    
    return(list(coef = result %>% t, p = result %>% t))
  }
  
  
  
  result <- data.frame(coefC = (zlm.out@coefC)[,2],  # second column = phenotypeH
                       coefD = (zlm.out@coefD)[,2],  # second column = phenotypeH
                       seC = zlm.out@vcovC %>% apply(3,diag) %>% "["(2,) %>% sqrt,
                       seD = zlm.out@vcovD %>% apply(3,diag) %>% "["(2,) %>% sqrt)
  df = c(1,1,1) # phenotype = 1df
  result %<>% transmute(cont = coefC^2/seC^2,
                        disc = coefD^2/seD^2,
                        hurdle = cont + disc)
  result.p = transmute(result,
                       pC = 1 - pchisq(cont, df[1]),
                       pD = 1 - pchisq(disc, df[2]),
                       pH = 1 - pchisq(hurdle, df[3]))
  return(list(coef = result %>% t, p = result.p %>% t))
}


if (FALSE) {# example
  data %>% MAST() -> tmp.a
  waldTest(tmp.a[[1]], Hypothesis('`phenotypeH`'))
  tmp.a@coefC
  
  
  data <- data.frame(x=rnorm(500), z=rbinom(500, 1, .3))
  logit.y <- with(data, x*2 + z*2); mu.y <- with(data, 10+10*x+10*z + rnorm(500))
  y <- (runif(500)<exp(logit.y)/(1+exp(logit.y)))*1
  y[y>0] <- mu.y[y>0]
  data$y <- y
  fit <- zlm(y ~ x+z, data)
  summary.glm(fit$disc)
  summary.glm(fit$cont)
}


### 8. KW
KW <- function (data, sig = 0.05) {
  require(coin)
  # log-transformation not needed for KW
  
  # fitting a nonparametric model
  out = kruskal_test(y ~ phenotype | batch, data = data)
  out = matrix(c(statistic(out), pvalue(out)), nrow = 1)
  colnames(out) = c("Estimate", "pval")
  return(out)
}

### 9. Wagner
Wagner <- function (data, sig = 0.05, zeroModel = c("logistic", "t.test", "lm")) {
  
  # Instead of proportion t-test, logistic regression is used to adjust for batch effect.
  # For nonzero model, modified WRS test (in coin package) is used. But it cannot handle small nonzero sample.
  
  # 1. zero model
  if (zeroModel[1] == "logistic") { # batch adjusted
    data.bin = data %>% mutate(y = ifelse(y>0, 1, 0))
    Z = (glm(y~phenotype + factor(batch), data=data.bin, family="binomial") %>% summary)$coef[2,3:4] %>% as.numeric
    #if phat = 0 or 1 (or all D and H are 0 or 1), then Z is automatically close to 0.
  } else if (zeroModel[1] == "t.test") {  # batch not adjusted!!!
    # table
    data %>% group_by(phenotype) %>% summarize(n = n(), n1 = sum(y!=0)) -> cont
    nD  = cont[1,2];  nH  = cont[2,2]
    nD1 = cont[1,3];  nH1 = cont[2,3]
    
    Z = abs(nD1/nD - nH1/nH) - .5/nD - .5/nH
    p.hat = (nD1 + nH1) / (nD + nH)
    Z = (Z / (p.hat*(1-p.hat)*(1/nD + 1/nH))^.5) %>% as.numeric
    
    if (p.hat*(1 - p.hat) == 0) {Z = 0}
    names(Z) <- NULL
    Z = c(Z, 2 - 2*pnorm(abs(Z %>% as.numeric)))
  } else if (zeroModel[1] == "lm") { # Just for comparison!!
    data.bin = data %>% mutate(y = ifelse(y>0, 1, 0))
    Z = (lm(y~phenotype + factor(batch), data=data.bin) %>% summary)$coef[2,3:4, drop=FALSE]
  } else stop("zeroModel was not correctly specified.")
  
  # return(c(z = Z, p = (1- pnorm(abs(as.numeric(Z))))*2))  
  
  # 2. nonzero model  
  # nonzero data
  data %>% dplyr::filter(y != 0) -> data.nonzero
  data.nonzero %>% dplyr::filter(phenotype=="D") %>% "$"("y") -> y.D
  data.nonzero %>% dplyr::filter(phenotype=="H") %>% "$"("y") -> y.H
  #print(y.D)
  
  # WRS test  
  # out = wilcox.test(x = y.D, y = y.H,
  #             alternative = c("two.sided"), correct = TRUE)
  # print(data.nonzero)
  # print("1. original WRS without batch")
  # wilcox.test(x = y.D, y = y.H,
  #             alternative = c("two.sided"), exact = FALSE, correct = TRUE) %>% print
  # print("2. modfified WRS without batch")
  # coin::wilcox_test(y ~ factor(phenotype), data=data.nonzero) %>% print
  
  # print("4. t-test WITH batch")
  # lm(y~phenotype+batch, data=data.nonzero) %>% print
  
  # print("3. modfified WRS WITH batch")
  W = try(coin::wilcox_test(y ~ factor(phenotype) | factor(batch), data=data.nonzero))
  if (any(class(W) %in% "try-error")) {
    W = matrix(NA, 1, 2)
  } else {
    W = matrix(c(statistic(W), pvalue(W)), nrow = 1)
  }
  
  colnames(W) = c("Estimate", "pval")
  
  # put together
  # print(Z)
  # print(W)
  chi2 = c(Z^2 + ifelse(is.na(W), 0, W)^2)
  chi2 = matrix(c(chi2, 1-pchisq(chi2, df=2)),1,2)
  
  out = rbind(W, Z, chi2) # nonzero, zero, global
  rownames(out) = c("Wg.nonz", "Wg.zero", "Wg.glob")
  rownames(out) = c("LB.nonz", "LB.zero", "LB.glob")
  
  return(out)
}

if (FALSE) {# example
  data %>% mutate(y=y.1) %>% Wagner()
  Wagner(data.1)
  Wagner(data.2)
  
  data %>% mutate(y=y.1) %>% Wagner(zeroModel = "logistic")
  data %>% mutate(y=y.1) %>% Wagner(zeroModel = "t.test")
  data %>% mutate(y=y.1) %>% Wagner(zeroModel = "lm")
  glm(y.1~phenotype, data=data) %>% summary
}
