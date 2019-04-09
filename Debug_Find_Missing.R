source("Script/C01.02.simulation.setup.R")
(parameter = parameter3); 
(delta = delta1); 
(kappa = kappa1)

length.delta = dim(delta)[1];
length.kappa = dim(kappa)[1];
length.parameter = dim(parameter)[1]


## 1.Check which report has problem
  
(Numer_of_missing(1,1,21, 15229890, length.kappa, length.parameter))


## 2.Check which reports are missing

miss = c("Miss files")
miss.i = c(); miss.j = c(); miss.k = c();
miss_num = c("ID of Missed Files")

for (i in 1:length.delta){ #i: delta 1:10
  for (j in 1:length.kappa) { #j: kappa 1:5
    for (k in 1:length.parameter) { #k: base params 1:40
      if(!file.exists(paste0("Output/R0201sim2019-03-24/result.",i,".",j,".",k,".rds"))){
        miss = c(miss, (paste0("Output/R0201sim2019-03-24/result.",i,".",j,".",k,".rds")))
        miss.i = c(miss.i,i); miss.j = c(miss.j,j); miss.k = c(miss.k,k);
        miss_num = c(miss_num, paste0("i=", i, " j=", j, " k=", k, " ",
          Numer_of_missing(i,j,k,14915967, length.kappa, length.parameter)))
      }
    }
  }
}

# 1. print error message
# 2. update gamlss 
# 3. debug the new function

Numer_of_missing <- function(i = 1, j = 1, k = 1, first_one = 0, 
                             j_num = 5, k_num = 62){
  out = (first_one-1) + (i-1)*(62*5) + (j-1)*62 + k
}
