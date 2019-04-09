### 0.1 library
library(dplyr); library(magrittr); library(ggplot2); library(gridExtra)
library(MASS); library(boot); library(pscl); library(R.utils)
source("Script/F00.00.generic.R")
source("Script/F01.01.base.R")

### 0.2 Data
# Raw data of 180 subjects
gene.marginal.RPK.DRNA <- readRDS("Data/data.geneRPK.marginal.DRNA.180.rds")
outcome <- readRDS("Data/data.outcome.180.rds") 
# gene.marginal.RPK.DRNA$otu[, , 2] is the RNA infomation

#Filter batch.DNA == 170421, and remove subject 253, 420
outcome.new = outcome[outcome$batch.DNA == 170421,]
outcome.new = outcome.new[outcome.new$id != 352 & outcome.new$id != 420,]
DataMeta116 = outcome.new

RNA = gene.marginal.RPK.DRNA$otu[, , 2]
RNA.new = RNA[,colnames(RNA) %in% DataMeta116$id]
DataRPK116 = RNA.new

rm(gene.marginal.RPK.DRNA, outcome, outcome.new, RNA, RNA.new)
############################################################################################
#Neew to throw out DNA info                        
#filter batches
############################################################################################


# # filtering 2 lowly-expressed subjects (352, 420)
# gene.marginal.RPK.DRNA %>% select(-RPK.352, -RPK.420) -> DataRPK116
# outcome %>% filter(!id %in% c(352,420)) -> DataMeta116
# rm(gene.marginal.RPK.RNA, outcome)

# # disease groups (0:Healthy, 1:Treated. 2:Diseased)
# HD = which(DataMeta116$ECC %in% c(0,2))
# H0 = which(DataMeta116$ECC == 0)
# T1 = which(DataMeta116$ECC == 1)
# D2 = which(DataMeta116$ECC == 2)
# 
# # batch
# B1 = which(DataMeta116$batch.RNA == "170628")
# B2 = which(DataMeta116$batch.RNA == "170718")


# Divide samples into groups 
HD = which(DataMeta116$cariesfree == 0) #disease
H0 = which(DataMeta116$cariesfree == 1) #healty

B1 = which(DataMeta116$batch.RNA == "170628")
B2 = which(DataMeta116$batch.RNA == "170718")

# HDB1 = which(DataMeta116$cariesfree == 0 & DataMeta116$batch.RNA == "170628")
# H0B1 = which(DataMeta116$cariesfree == 1 & DataMeta116$batch.RNA == "170628")

# subject_HD = DataMeta116[ DataMeta116$cariesfree == 0,]
# subject_H0 = DataMeta116[ DataMeta116$cariesfree == 1,]
# sample_HD = DataRPK116[,colnames(DataRPK116) %in% subject_HD$id]
# sample_H0 = DataRPK116[,colnames(DataRPK116) %in% subject_H0$id]
# 
# subject_B1 = DataMeta116[ DataMeta116$batch.RNA == 170628,]
# subject_B2 = DataMeta116[ DataMeta116$batch.RNA == 170718,]
# sample_B1 = DataRPK116[,colnames(DataRPK116) %in% subject_B1$id]
# sample_B2 = DataRPK116[,colnames(DataRPK116) %in% subject_B2$id]

subject_H1 = DataMeta116[ DataMeta116$cariesfree == 1 & DataMeta116$batch.RNA == "170628",]
subject_H2 = DataMeta116[ DataMeta116$cariesfree == 1 & DataMeta116$batch.RNA == "170718",]
subject_D1 = DataMeta116[ DataMeta116$cariesfree == 0 & DataMeta116$batch.RNA == "170628",]
subject_D2 = DataMeta116[ DataMeta116$cariesfree == 0 & DataMeta116$batch.RNA == "170718",]
set.seed(1)
samp = sample(1:nrow(DataRPK116),1000)
sample_H1 = DataRPK116[samp,colnames(DataRPK116) %in% subject_H1$id]
sample_H2 = DataRPK116[samp,colnames(DataRPK116) %in% subject_H2$id]
sample_D1 = DataRPK116[samp,colnames(DataRPK116) %in% subject_D1$id]
sample_D2 = DataRPK116[samp,colnames(DataRPK116) %in% subject_D2$id]

#ZINB.ML takes too long

# plotly to creat 3d plot
# Divide into H1 H1 D1 D1 4 groups
# for each gene, compute (mu,theta, pi) for each group
# draw graph of all genes to have a look of each parameter, also 2d/3d plot

dataExamine <- function(data){
  mu = character(0)
  theta = character(0)
  pi = character(0)
  check = character(0)
  i_mu = 1; i_theta = 1; i_pi = 1;i_check = 1;
  
  for(i in 1:dim(data)[1]){
    if(i%%10==0){print(i)}
    y.vector = as.vector(data[i,])
    if(max(y.vector)>0){
      result = ZINB.ML.time(y.vector)
    }
    if(!is.nan(result[1]) & !is.nan(result[2]) & !is.nan(result[3]) &
       !is.na(result[1]) & !is.na(result[2]) & !is.na(result[3]) ){
      mu[i_mu] = result[1]
      i_mu = i_mu+1;
      
      theta[i_theta] = result[2]
      i_theta = i_theta+1;
      
      pi[i_pi] = result[3]
      i_pi = i_pi+1;
    }
  }
  return (list(as.numeric(mu),as.numeric(theta),as.numeric(pi)))
}




result_H1 = dataExamine(sample_H1)

result_H2 = dataExamine(sample_H2)

result_D1 = dataExamine(sample_D1)

result_D2 = dataExamine(sample_D2)

saveRDS(result_H1, "parameters/result_H1.rds")
saveRDS(result_H2, "parameters/result_H2.rds")
saveRDS(result_D1, "parameters/result_D1.rds")
saveRDS(result_D2, "parameters/result_D2.rds")

####Analyze H1
## mu
summary(unlist(result_H1[1]))
boxplot(unlist(result_H1[1]), main = "mu of H1")
boxplot(unlist(result_H1[1])[unlist(result_H1[1])<1000], main = "mu of H1(>1000 omitted)")
boxplot(unlist(result_H1[1])[unlist(result_H1[1])<100], main = "mu of H1(>100 omitted)")
boxplot(unlist(result_H1[1])[unlist(result_H1[1])<40], main = "mu of H1(>40 omitted)")
plot(hist(unlist(result_H1[1])[unlist(result_H1[1])<40]),main = "mu of H1(>40 omitted)", xlab = "mu")
## theta

## pi

####Analyze H2

####Analyze D1

####Analyze D2

# Comparing between HD and H0, mu_D > mu_H, theta_D > theta_H, pi_D < pi_H
# Comparing between B1 and B2, mu_1 < mu_2, theta_1 > theta_2, pi_1 > pi_2

### 1. parameter estimates from real data (ZINB): baseline parameters
if (FALSE) {
  param = data.frame(gene.id = NA, mu=NA, theta=NA, pi=NA)
  n = dim(DataRPK116)[1]
  
  tt(1); k=1
  for (i in 1:n) {
    if (i %% 100) next  #only every 100 other genes
    if (!i %% 3000) cat(i," out of ",n, "genes (", round(i/n*100,2),"%), expected time = ",
                        (Sys.time()-time.tmp)/i*n, "minutes.")
    param[k,] = c(i, DataRPK116[i,-1] %>% round %>% ZINB.ML.time(notation="mtp"))
    k = k+1
  }
  tt(2)  #14 mins for 2,386 genes
  saveRDS(param, "~/Desktop/Microbiome-master/output/R0101.param.rds")
} else {
  param <- readRDS("output/R0101.param.rds")
  n = dim(DataRPK116)[1] 
}

## 1.1 resulting figures ####
param %>% apply(2, mean, na.rm=T)
param %>% ggplot(aes(mu, theta, col=pi)) + geom_point()

# parameters without outliers
param.outlier = which(param$theta > 1e+2)
param [param.outlier,]
DataRPK116[119100,-1] %>% as.numeric %>% round %>% hist
length(param.outlier) # 714 out of 2,386 genes have theta > 100

param %>% filter(theta<=100, mu<500) %>% ggplot(aes(theta, mu, col=pi)) + geom_point() + ggtitle("theta<100")
ggsave("Document/plot/P0101-param-mtp.png")
saveRDS(param, "param.rds")

param %>% filter(theta<100) %>% ggplot(aes(theta)) + geom_density() 
# param %>% filter(theta<100) %>% ggplot(aes(theta)) + geom_density() + xlim(c(0,1)); 
# mode at theta=.1
ggsave("Document/plot/P0101-param-t.png")

param %>% filter(theta <=100, mu<500) %>% ggplot(aes(mu)) + geom_density()
#param %>% filter(theta <=100) %>% ggplot(aes(mu)) + geom_density() + xlim(c(0,3));
ggsave("Document/plot/P0101-param-m.png")
# mode at mu=1

param %>% filter(theta <=100) %>% ggplot(aes(pi)) + geom_density()
ggsave("Document/plot/P0101-param-p.png")
# uniform [0,1] + uniform [0.75,1] with 50% chance


### 2. parameter estimates from real data (ZINB): delta (H-D group differences)
if (FALSE) {
  param.ECC = data.frame(gene.id = NA, 
                         mu0=NA, theta0=NA, pi0=NA, 
                         mu1=NA, theta1=NA, pi1=NA,
                         mu2=NA, theta2=NA, pi2=NA)
  tt(1); k=1
  for (i in 1:n) {
    if (i %% 100) next  #only every 100 other genes
    if (!i %% 3000) cat(i," out of ",n, "genes (", round(i/n*100,2),"%), expected time = ",
                        (Sys.time()-time.tmp)/i*n, "minutes.")
    param.ECC[k,] = c(i, ZINB.ML.time(DataRPK116[i,H0+1]),
                      ZINB.ML.time(DataRPK116[i,T1+1]), ZINB.ML.time(DataRPK116[i,D2+1]))
    # filling in pi's with the proportion of zero counts, if the genes are not estimated.
    if (is.na(param.ECC[k,4])) {param.ECC[k,4] = mean(DataRPK116[i,H0+1]==0)}
    if (is.na(param.ECC[k,7])) {param.ECC[k,7] = mean(DataRPK116[i,T1+1]==0)}
    if (is.na(param.ECC[k,10])) {param.ECC[k,10] = mean(DataRPK116[i,D2+1]==0)}
    k = k+1
  }
  tt(2)  # 26 mins for 2,386 genes
  saveRDS(param.ECC, "output/R0101.param.ECC.rds")
} else {
  param.ECC <- readRDS("output/R0101.param.ECC.rds")
}   


## 2.1 error analysis ####   
# NaN: estimation error
# NA: timeouts
apply(param.ECC, 2, function(x) mean(is.nan(x))) %>% round(2) -> tmp.NaN
apply(param.ECC, 2, function(x) mean(is.na(x))) %>% round(2) -> tmp.NA
tmp.NA = tmp.NA - tmp.NaN #NA includes NaN. Thus subtracted.
rbind(tmp.NaN, tmp.NA)[,c(2,5,8)]
#           H      T      D
# tmp.NaN   0.32   0.43   0.24
# tmp.NA    0.09   0.06   0.11

# when all (or all but a few) are zero counts, error
# when the few nonzero counts are large numbers, error (NaN)
# when the few nonzero counts are small numbers, time out (NA)

param.ECC[is.nan(param.ECC[,2]),1] %>% sapply(function(s) {mean(DataRPK116[s,H0+1]==0)})
param.ECC[is.nan(param.ECC[,5]),1] %>% sapply(function(s) {mean(DataRPK116[s,T1+1]==0)})
param.ECC[is.nan(param.ECC[,8]),1] %>% sapply(function(s) {mean(DataRPK116[s,D2+1]==0)})
# Seen that most of the NaN are mostly zero counts
param.ECC[(!is.nan(param.ECC[,2])) & is.na(param.ECC[,2]) ,1] %>% sapply(function(s) {mean(DataRPK116[s,H0+1]==0)})
DataRPK116[1200,H0+1] %>% round %>% sort %>% as.numeric # example of timeout counts
DataRPK116[2300,H0+1] %>% round %>% sort %>% as.numeric # example of timeout counts
DataRPK116[3000,H0+1] %>% round %>% sort %>% as.numeric # example of timeout counts

# NaN and NA are very similar in that there are overwelming # of zero's.
# -> For those NaN and NA, pi's are filled in with the zero-proportion.

## 2.2 delta (ratio) #####
## delta.theta
data_frame(x=param.ECC[,2],y=param.ECC[,8], delta=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(delta) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,900,1000,1200))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp

log(tmp$delta) %>%  summary # 3Q: 0.615

tmp %>% ggplot(aes(index, delta)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("delta_theta of real data")
ggsave("Document/plot/P0102-param-delta-t.png")
# suggests 5?

# distribution among reasonable parameter values (theta<=100)
tmp %<>% filter(x<=100, y<=100) 
tmp %>% ggplot(aes(x=delta)) + geom_histogram() + xlab("delta_theta") + ggtitle("theta<=100")
ggsave("Document/plot/P0102-param-delta-t100.png")
tmp %>% filter(delta %btw% c(5,30)) %>% ggplot(aes(x,y, col=delta)) + geom_point() +
  ggtitle("theta estimate distribution where delta is between 5 and 30, theta<=100")+ xlab("theta(ECC0)") + ylab("theta(ECC2)")
ggsave("Document/plot/P0102-param-delta-t100b.png")  


## delta.mu
data_frame(x=param.ECC[,3],y=param.ECC[,9], delta=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(delta) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,800,1000,1200))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp

log(tmp$delta) %>%  summary # 3Q: 3.66

tmp %>% ggplot(aes(index, delta)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("delta_mu of real data")
ggsave("Document/plot/P0102-param-delta-m.png")
# suggests 20?

## distribution among reasonable parameter values (mu<=100)
tmp %<>% filter(x<=100, y<=100)
tmp %>% ggplot(aes(x=delta)) + geom_histogram() + xlab("delta_mu")  + ggtitle("mu<=100")
ggsave("Document/plot/P0102-param-delta-m100.png")
tmp %>% filter(delta %btw% c(5,30)) %>% ggplot(aes(x, y, col=delta)) + geom_point() +
  ggtitle("mu estimate distribution where delta is between 5 and 30, mu<=100") + xlab("mu(ECC0)") + ylab("mu(ECC2)")
ggsave("Document/plot/P0102-param-delta-m100b.png")



## delta.pi
data_frame(x=param.ECC[,4],y=param.ECC[,10], delta=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(delta) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,2200,2300,3000))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp
tmp %>% transmute(delta = qlogis(x) - qlogis(y)) %>% abs  %>% filter(is.finite(delta)) %>% summary
# 3Q = 0.99

tmp %>% ggplot(aes(index, delta)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("delta_pi of real data")
ggsave("Document/plot/P0102-param-delta-p.png")
# suggests 3?

## distribution among reasonable parameter values (pi>1e-4)
tmp %<>% filter(x>1e-4, y>1e-4)
tmp %>% ggplot(aes(x=delta)) + geom_histogram() + xlab("delta_pi")  + ggtitle("pi>1e-4")
ggsave("Document/plot/P0102-param-delta-p100.png")
tmp %>% ggplot(aes(x, y, col=delta)) + geom_point() +
  ggtitle("pi estimate distribution for all delta, pi>1e-4") + xlab("pi(ECC0)") + ylab("pi(ECC2)")
ggsave("Document/plot/P0102-param-delta-p100b.png")



### 3. parameter estimates from real data (ZINB): kappa (batch effects)    
if (FALSE) {
  param.bacth = data.frame(gene.id = NA, 
                           mu1=NA, theta1=NA, pi1=NA, 
                           mu2=NA, theta2=NA, pi2=NA)
  tt(1); k=1
  for (i in 1:n) {
    if (i %% 100) next  #only every 100 other genes
    if (!i %% 3000) cat(i," out of ",n, "genes (", round(i/n*100,2),"%), expected time = ",
                        (Sys.time()-time.tmp)/i*n, "minutes.")
    param.bacth[k,] = c(i, ZINB.ML.time(DataRPK116[i,B1+1]), ZINB.ML.time(DataRPK116[i,B2+1]))
    # filling in pi's with the proportion of zero counts, if the genes are not estimated.
    if (is.na(param.bacth[k,4])) {param.bacth[k,4] = mean(DataRPK116[i,B1+1]==0)}
    if (is.na(param.bacth[k,7])) {param.bacth[k,7] = mean(DataRPK116[i,B2+1]==0)}
    k = k+1
  }
  tt(2)  # 39 mins for 2,386 genes
  saveRDS(param.bacth, "output/R0101.param.batch.rds")
} else {
  param.bacth <- readRDS("output/R0101.param.batch.rds")
}

## 3.2 kappa (ratio) #####
## kappa.theta
data_frame(x=param.bacth[,2],y=param.bacth[,5], kappa=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(kappa) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,800,1100,3000))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp

log(tmp$kappa) %>%  summary # Med: 0.57

tmp %>% ggplot(aes(index, kappa)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("kappa_theta of real data")
ggsave("Document/plot/P0102-param-kappa-t.png")

log(tmp$kappa) %>% mean %>% exp #1.8
# suggests 2

# distribution among reasonable parameter values (theta<=100)
tmp %<>% filter(x<=100, y<=100) 
tmp %>% ggplot(aes(x=kappa)) + geom_histogram() + xlab("kappa_theta") + ggtitle("theta<=100")
ggsave("Document/plot/P0102-param-kappa-t100.png")
tmp %>% filter(kappa %btw% c(5,30)) %>% ggplot(aes(x,y, col=kappa)) + geom_point() +
  ggtitle("theta estimate distribution where kappa is between 5 and 30, theta<=100")+ xlab("theta(ECC0)") + ylab("theta(ECC2)")
ggsave("Document/plot/P0102-param-kappa-t100b.png")  
log(tmp$kappa) %>% mean %>% exp #1.7
# suggests 2

## kappa.mu
data_frame(x=param.ECC[,3],y=param.ECC[,6], kappa=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(kappa) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,650,850,1000))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp

log(tmp$kappa) %>%  summary # Med: 1.07

tmp %>% ggplot(aes(index, kappa)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("kappa_mu of real data")
ggsave("Document/plot/P0102-param-kappa-m.png")
log(tmp$kappa) %>% mean %>% exp #34
# suggests 30?

## distribution among reasonable parameter values (mu<=100)
tmp %<>% filter(x<=100, y<=100)
tmp %>% ggplot(aes(x=kappa)) + geom_histogram() + xlab("kappa_mu")  + ggtitle("mu<=100")
ggsave("Document/plot/P0102-param-kappa-m100.png")
tmp %>% filter(kappa %btw% c(5,30)) %>% ggplot(aes(x, y, col=kappa)) + geom_point() +
  ggtitle("mu estimate distribution where kappa is between 5 and 30, mu<=100") + xlab("mu(ECC0)") + ylab("mu(ECC2)")
ggsave("Document/plot/P0102-param-kappa-m100b.png")
log(tmp$kappa) %>% mean %>% exp #2.5


## kappa.pi
data_frame(x=param.ECC[,4],y=param.ECC[,7], kappa=pmax(y/x, x/y)) %>% 
  na.omit %>% arrange(kappa) %>% mutate(index = 1:n()) %>% 
  mutate(facet=cut(index, c(0,2200,2250,3000))) %>%
  mutate(facet=factor(facet, levels = levels(facet)[3:1])) -> tmp

tmp %>% transmute(kappa = qlogis(x) - qlogis(y)) %>% abs  %>% filter(is.finite(kappa)) %>% summary
# Median = 0.72

tmp %>% ggplot(aes(index, kappa)) + geom_point() + facet_grid(facet ~., scale="free") +
  ggtitle ("kappa_pi of real data")
ggsave("Document/plot/P0102-param-kappa-p.png")
log(tmp$kappa) %>% mean %>% exp #Inf
# suggests 2?

## distribution among reasonable parameter values (pi>1e-4)
tmp %<>% filter(x>1e-4, y>1e-4)
tmp %>% ggplot(aes(x=kappa)) + geom_histogram() + xlab("kappa_pi")  + ggtitle("pi>1e-4")
ggsave("Document/plot/P0102-param-kappa-p100.png")
tmp %>% ggplot(aes(x, y, col=kappa)) + geom_point() +
  ggtitle("pi estimate distribution for all kappa, pi>1e-4") + xlab("pi(ECC0)") + ylab("pi(ECC2)")
ggsave("Document/plot/P0102-param-kappa-p100b.png")
log(tmp$kappa) %>% mean %>% exp #1.12



######## exercise
if (FALSE) {
  DataRPK116[5,-1] %>% as.numeric %>% round %>% apply(1, ZINB.ML)
  DataRPK116[5,-1] %>% as.numeric %>% round %>% hist
  DataRPK116[5,-1] %>% as.numeric %>% round %>% ZINB.ML.time (timeout=1)
  DataRPK116[6,-1] %>% as.numeric %>% round %>% ZINB.ML.time (timeout=1)
  
  # running 10 genes for example
  param = DataRPK116[1:10,-1] %>% round %>% apply(1, ZINB.ML.time) %>% t
  param = structure(param, dimnames=list(1:10, c("theta", "mu", "pi")))
  
}

