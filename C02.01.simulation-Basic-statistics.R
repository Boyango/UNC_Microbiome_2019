source("Script/C01.02.simulation.setup.R")
source("Script/F02.01.simulation-analysis.R")
##install.packages("gamlss")
library(gamlss)
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra)

(parameter = parameter3); 
(delta = delta1); 
(kappa = kappa1)

length.delta = dim(delta)[1];
length.kappa = dim(kappa)[1];
length.parameter = dim(parameter)[1]


lapply(1:length.delta, function(i) { #i: delta 1:10
  lapply(1:length.kappa, function(j) { #j: kappa 1:5
    lapply(1:length.parameter, function(k) { #k: base params 1:40
      #readRDS(paste0("output/R0201sim181201/result.",i,".",j,".",k,".rds"))
      if(file.exists(paste0("Output/R0201sim2019-03-05/result.",i,".",j,".",k,".rds"))){
        readRDS(paste0("Output/R0201sim2019-03-11/result.",i,".",j,".",k,".rds"))
      }else{
        readRDS(paste0("Output/R0201sim2019-03-11/result.",i,".",j,".",k,".rds"))
      }
    })
  })
}) -> result



result[[1]][[1]][[1]]$pval
sig = 0.05

### replacing NA values in global with non.NA from other models!!!!!
result[[1]][[1]][[10]]$pval[5:7,1:20]

for (i in 1:length.delta) {
  for (j in 1:length.kappa) {
    for (k in 1:length.parameter) {
      # step 1. getting NA addresses
      na.index = result[[i]][[j]][[k]]$pval[7,] %>% is.na %>% which
      # step 2. replacing with nonzero model values
      result[[i]][[j]][[k]]$pval[7,na.index] = result[[i]][[j]][[k]]$pval[6,na.index]
      # step 3. getting NA addresses again and replace with zero model values.
      na.index = result[[i]][[j]][[k]]$pval[7,] %>% is.na %>% which
      result[[i]][[j]][[k]]$pval[7,na.index] = result[[i]][[j]][[k]]$pval[5,na.index]
      # leftovers
      # result[[i]][[j]][[k]]$pval[7,na.index] %>% length %>%"/"(n.sim) %T>% print
    }
  }
}


### getting stats (power and type-I error)
# generating empty slots
a <- c(i=1, j=1, k=1, result[[1]][[1]][[1]]$pval %>% apply(1, function(x) {mean(x<=0.05)}) )
a[-(1:3)] <- NA
result.stat.na <- result.stat <- NA.proportion <-  matrix(a, nrow = 10*5*46, ncol = length(a), byrow = TRUE, 
                                                          dimnames = list(NULL, names(a))) %>% as.data.frame

# filling in
row.index = 1
for (i in 1:length.delta) {
  for (j in 1:length.kappa) {
    for (k in 1:length.parameter) {
      result.stat[row.index,] = c(i=i, j=j, k=k, result[[i]][[j]][[k]]$pval %>% apply(1, function(x) {mean(x<=0.05, na.rm=TRUE)}) )
      result.stat.na[row.index,] = 
        c(i=i, j=j, k=k, result[[i]][[j]][[k]]$pval %>% apply(1, function(x) {mean(ifelse(is.na(x), 1, x)<=0.05, na.rm=TRUE)}) )
      NA.proportion[row.index,] =
        c(i=i, j=j, k=k, result[[i]][[j]][[k]]$pval %>% apply(1, function(x) {mean(is.na(x))}))
      row.index = row.index + 1
    }
  }  
}
#
result.stat = result.stat %>% mutate(i, effect = delta[i, 4], j, batch = kappa[j,4])
result.stat.na = result.stat.na %>% mutate(i, effect = delta[i, 4], j, batch = kappa[j,4])
NA.proportion = NA.proportion %>% mutate(i, effect = delta[i, 4], j, batch = kappa[j,4])

# rounding numbers
if (FALSE) {
  result.stat %<>% mutate(LB.nonz = round(LB.nonz, 3), LB.zero = round(LB.zero, 3),
                          LB.glob = round(LB.glob, 3),
                          LN = round(LN, 3), KW = round(KW, 3),
                          MAST.nonz = round(MAST.nonz, 3), MAST.zero = round(MAST.zero, 3),
                          MAST.glob = round(MAST.glob, 3),
                          Wg.nonz = round(Wg.nonz, 3), Wg.zero = round(Wg.zero, 3),
                          Wg.glob = round(Wg.glob, 3))
}

# Create the folder with the time of creating plots
save_path = paste0("Document/plot/", Sys.Date(), "/")
if (!file.exists(save_path)) {
  dir.create(file.path(save_path))
}

for (test in method.stat) {
  a <- lapply(c(dim(delta)[1]+1, 1:(dim(delta)[1])), function(i) pval.plot(result.stat,
                                                                           parameter_in_use = parameter, i=i, test=test))
  a <- marrangeGrob(a, nrow=4, ncol=2)
  #ggsave(paste0("Document/plot/P1101",test,".pdf"), a, width = 10, height=12)
  ggsave(paste0(save_path, "P1101.10K.",test,".pdf"), a, width = 10, height=12)
}
for (test in method.stat) {
  a <- lapply(c(dim(delta)[1]+1, 1:(dim(delta)[1])), function(i) pval.plot(NA.proportion, 
                                                                           parameter_in_use = parameter,
                                                                           i=i, test=test, title=paste0("NA proportion of ", test)))
  a <- marrangeGrob(a, nrow=4, ncol=2)
  #ggsave(paste0("Document/plot/P1101",test,".pdf"), a, width = 10, height=12)
  ggsave(paste0(save_path, "P1101.10K.na.proportion.",test,".pdf"), a, width = 10, height=12)
}


## reduced plots
#1. null effects
a <- lapply(c("LB.glob", "LN", "MAST.glob", "KW",  "Wg.glob"), 
            function(test) pval.plot(result.stat.na, 
                                     parameter_in_use = parameter,
                                     i=1, test=test, 
                                     k.index= c(2,3,5,6,11,12,14,15),
                                     j.index= c(1,3), ylim = c(0,0.3),
                                     title = test.name[test == test.name[,1],2] %>% gsub(" \\- .*", "", .) ))
a <- marrangeGrob(a, nrow=5, ncol=1)
ggsave(paste0(save_path, "P1101.reduced.10K.null.pdf"), a, width = 5, height=12)

#2. differential effects
for (test in c("LB.glob", "LN", "MAST.glob", "KW",  "Wg.glob")) {
  a <- lapply(2:7, function(i) pval.plot(result.stat.na, 
                                         parameter_in_use = parameter,
                                         i=i, test=test, 
                                         k.index= c(2,3,5,6,11,12,14,15),
                                         j.index= c(1,3)))
  a <- marrangeGrob(a, nrow=3, ncol=2)
  ggsave(paste0(save_path, "P1101.reduced.10K.",test,".pdf"), a, width = 10, height=12)
}

# NA proprtions are almost the same across methods
NA.proportion %>% dplyr::filter(j%in%c(1,3), k%in% c(2,3,5,6,11,12,14,15)) %>% dplyr::select(i, j,  k, LB.glob, MAST.glob, LN, KW, Wg.glob, effect, batch)

if (FALSE) {
  result.stat.1 %>% dplyr::filter(i==1) # null effect            
  # LB suffers from zero-inflation(k=10~27)
  # LN and KW is robust to batch effects, but LB is not.
  
  result.stat.1 %>% dplyr::filter(i==2) # mean shift (nonzero)
  # LN has generally high power than KW
  # For high zero-inflation (p=0.95), LN and KW suffers (power = alpha),
  # For extreme zero-inflation (p=0.99), LN and KW has no power
  # KW suffers more than LN as zero-inflation gets higher.
  
  # When batch effect gets larger, overall power decreases, but KW suffers more.
  
  
  result.stat.1 %>% dplyr::filter(i==3) # scale effect (keeping mean the same)
  result.stat.1 %>% dplyr::filter(i==4) # zero inflation
  result.stat.1 %>% dplyr::filter(i==5) # mean + scale
  result.stat.1 %>% dplyr::filter(i==6) # mean + zero inflation
  result.stat.1 %>% dplyr::filter(i==7) # scale + zero inflation
}

