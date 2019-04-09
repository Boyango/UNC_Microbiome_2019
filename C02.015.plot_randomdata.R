

## 0. Set parameters and seed 
# parameter1
expand.grid(m=c(2, 3, 5, 10), t=c(0.5, 1), p=c(.3, .5, .6, .9, .95)) %>% # normal scenarios 1-40
  rbind(expand.grid(m=c(2, 3, 5), t=c(5), p=c(.9, .95))) %>%  # extreme scenario 41-46
  data.frame() %>%
  mutate(no = 1:n()) %>%       # add scenario numbers (no)
  dplyr::select(no, everything())  -> # reorder columns
  parameter1
# delta = differential expression
matrix(c(0, 0, 0,
         1, 0, 0,  0, 1,  0,    0, 0, -1,
         1, 1, 0,  1, 0, -1,    0, 1, -1,
         1, 0, 1,  1, -1, 0,    0, -1, -1), byrow = TRUE,
       nrow = 10, dimnames=list(1:10, c("m", "t", "p"))) %>% data.frame -> delta
delta$detail = paste0("Effect_", c("null", "mu(D>H)", "theta(D>H)", "pi(D<H)", 
                                   "mu(D>H).theta(D>H)", "mu(D>H).pi(D<H)", "theta(D>H).pi(D<H)",
                                   "mu(D>H),pi(D>H)","mu(D>H).theta(D<H)","theta(D<H).pi(D<H)"))
# kappa
matrix(c(0, 0, 0,   .5, -.5, -.5,   1, -1, -1,   .5, .5, -.5,   1, 1, -1), byrow = TRUE,
       nrow = 5, dimnames=list(1:5, c("m", "t", "p"))) %>% data.frame -> kappa
kappa$detail = paste(c("no","small(+,-,-)", "large(+,-,-)", "small(+,+,-)", "large(+,+,-)"), "batch effect")
# define i,j,k
i = 2; j = 1; k = 1
# set seed 
set.seed(i*10^2 + j*10 + k)

## 1. parameter
param.set = param (i, j, k, 
                   baseParam = parameter1,
                   delta.table = delta, 
                   kappa.table = kappa) # list of H1, D1, H2, D2 (status-batch)

## 2. generate random data
data = rZINB.sim(n.sample = n.sample, n.genes=n.genes, scenario.delta = i, scenario.kappa = j, scenario.base = k)

## 3. analysis
data_matrix = as.matrix(data)

data_matrix[ 1:20, ] %>% as.numeric() %>% na.omit() %>% as.numeric() -> g1
data_matrix[21:40, ] %>% as.numeric() %>% na.omit() %>% as.numeric() -> g2
data_matrix[41:60, ] %>% as.numeric() %>% na.omit() %>% as.numeric() -> g3
data_matrix[61:80, ] %>% as.numeric() %>% na.omit() %>% as.numeric() -> g4

summary(g1); 
summary(g2); 
summary(g3); 
summary(g4)

# Create the folder with the time of creating plots
save_path = paste0("Document/plot_randomdata/", Sys.Date(), "/")
if (!file.exists(save_path)) {
  dir.create(file.path(save_path))
}


plot(g1, main = "H1")
plot(g2, main = "D1")
plot(g3, main = "H2")
plot(g4, main = "D2")




data.frame(y = data[,2], group = rep(c("H1", "D1", " H2", "D2"), n.sample)) %>% 
  mutate(y.comp = y/ifelse(sum(y) ==0 , 1, sum(y))) %>% 
  ggplot(aes(x = group, y = y.comp)) + 
  geom_jitter(width = 0.2, height=0)


