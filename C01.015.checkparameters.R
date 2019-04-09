
library(plotly); library(dplyr)
result_H1 = readRDS("output/Grouped Sample/result_H1.rds")
result_H2 = readRDS("output/Grouped Sample/result_H2.rds")
result_D1 = readRDS("output/Grouped Sample/result_D1.rds")
result_D2 = readRDS("output/Grouped Sample/result_D2.rds")

####Analyze H1
check = result_H1
## mu
summary(unlist(check[1]))
boxplot(unlist(check[1]), main = "mu of H1")
boxplot(unlist(check[1])[unlist(check[1])<1000], main = "mu of H1(>1000 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<100], main = "mu of H1(>100 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of H1(>40 omitted)")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of H1(>40 omitted)", xlab = "mu")

jpeg(filename="parameters/H1_mu_Hist.jpg")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of H1(>40 omitted)", xlab = "mu")
dev.off()
jpeg(filename="parameters/H1_mu_BoxP.jpg")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of H1(>40 omitted)")
dev.off()

## theta
summary(unlist(check[2]))
boxplot(unlist(check[2]), main = "theta of H1")
boxplot(unlist(check[2])[unlist(check[2])<10^10], main = "theta of H1(>10^10 omitted)")
boxplot(unlist(check[2])[unlist(check[2])<10^2], main = "theta of H1(>10^2 omitted)")
plot(hist(unlist(check[2])[unlist(check[2])<10^2]),main = "theta of H1(>10^2 omitted)", xlab = "theta")
#half the thetas are below 100

jpeg(filename="parameters/H1_theta_Hist.jpg")
plot(hist(unlist(check[2])[unlist(check[2])<10^2]),main = "theta of H1(>10^2 omitted)", xlab = "theta")
dev.off()
jpeg(filename="parameters/H1_theta_BoxP.jpg")
boxplot(unlist(check[2])[unlist(check[2])<10^2], main = "theta of H1(>10^2 omitted)")
dev.off()

## pi
summary(unlist(check[3]))
boxplot(unlist(check[3]), main = "pi of H1")
plot(hist(unlist(check[3])), main = "pi of H1", xlab = "pi")

jpeg(filename="parameters/H1_pi_Hist.jpg")
plot(hist(unlist(check[3])), main = "pi of H1", xlab = "pi")
dev.off()
jpeg(filename="parameters/H1_pi_BoxP.jpg")
boxplot(unlist(check[3]), main = "pi of H1")
dev.off()

##scatterplot for H1 group
check = result_H1

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^2], 
     unlist(check[2])[unlist(check[1])<40 &unlist(check[2])<10^2],
     xlab = "mu of H1(>40 omitted)", ylab = "theta of H1(>10^2 omitted)")

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^2], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^2],
     xlab = "mu of H1(>40 omitted)", ylab = "pi of H1")

plot(unlist(check[2])[unlist(check[1])<40 & unlist(check[2])<10^2], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^2],
     xlab = "theta of H1(>10^2 omitted)", ylab = "pi of H1")

####Analyze H2
check = result_H2
## mu
summary(unlist(check[1]))
boxplot(unlist(check[1]), main = "mu of H2")
boxplot(unlist(check[1])[unlist(check[1])<1000], main = "mu of H2(>1000 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<100], main = "mu of H2(>100 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of H2(>40 omitted)")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of H2(>40 omitted)", xlab = "mu")

jpeg(filename="parameters/H2_mu_Hist.jpg")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of H2(>40 omitted)", xlab = "mu")
dev.off()
jpeg(filename="parameters/H2_mu_BoxP.jpg")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of H2(>40 omitted)")
dev.off()

## theta
summary(unlist(check[2]))
boxplot(unlist(check[2]), main = "theta of H2")
boxplot(unlist(check[2])[unlist(check[2])<10^10], main = "theta of H2(>10^10 omitted)")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of H2(>10^1 omitted)")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of H2(>10^1 omitted)", xlab = "theta")

jpeg(filename="parameters/H2_theta_Hist.jpg")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of H2(>10^1 omitted)", xlab = "theta")
dev.off()
jpeg(filename="parameters/H2_theta_BoxP.jpg")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of H2(>10^1 omitted)")
dev.off()

## pi
summary(unlist(check[3]))
boxplot(unlist(check[3]), main = "pi of H2")
plot(hist(unlist(check[3])), main = "pi of H2", xlab = "pi")

jpeg(filename="parameters/H2_pi_Hist.jpg")
plot(hist(unlist(check[3])), main = "pi of H2", xlab = "pi")
dev.off()
jpeg(filename="parameters/H2_pi_BoxP.jpg")
boxplot(unlist(check[3]), main = "pi of H2")
dev.off()

##scatterplot for H2 group
check = result_H2

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[2])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of H2(>40 omitted)", ylab = "theta of H2(>10^1 omitted)")

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of H2(>40 omitted)", ylab = "pi of H2")

plot(unlist(check[2])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "theta of H2(>10^1 omitted)", ylab = "pi of H2")


####Analyze D1
check = result_D1
## mu
summary(unlist(check[1]))
boxplot(unlist(check[1]), main = "mu of D1")
boxplot(unlist(check[1])[unlist(check[1])<1000], main = "mu of D1(>1000 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<100], main = "mu of D1(>100 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of D1(>40 omitted)")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of D1(>40 omitted)", xlab = "mu")

jpeg(filename="parameters/D1_mu_Hist.jpg")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of D1(>40 omitted)", xlab = "mu")
dev.off()
jpeg(filename="parameters/D1_mu_BoxP.jpg")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of D1(>40 omitted)")
dev.off()

## theta
summary(unlist(check[2]))
boxplot(unlist(check[2]), main = "theta of D1")
boxplot(unlist(check[2])[unlist(check[2])<10^10], main = "theta of D1(>10^10 omitted)")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of D1(>10^1 omitted)")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of D1(>10^1 omitted)", xlab = "theta")

jpeg(filename="parameters/D1_theta_Hist.jpg")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of D1(>10^1 omitted)", xlab = "theta")
dev.off()
jpeg(filename="parameters/D1_theta_BoxP.jpg")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of D1(>10^1 omitted)")
dev.off()

## pi
summary(unlist(check[3]))
boxplot(unlist(check[3]), main = "pi of D1")
plot(hist(unlist(check[3])), main = "pi of D1", xlab = "pi")

jpeg(filename="parameters/D1_pi_Hist.jpg")
plot(hist(unlist(check[3])), main = "pi of D1", xlab = "pi")
dev.off()
jpeg(filename="parameters/D1_pi_BoxP.jpg")
boxplot(unlist(check[3]), main = "pi of D1")
dev.off()

##scatterplot for D1 group
check = result_D1

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[2])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of D1(>40 omitted)", ylab = "theta of D1(>10^1 omitted)")

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of D1(>40 omitted)", ylab = "pi of D1")

plot(unlist(check[2])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "theta of D1(>10^1 omitted)", ylab = "pi of D1")


####Analyze D2
check = result_D2
## mu
summary(unlist(check[1]))
boxplot(unlist(check[1]), main = "mu of D2")
boxplot(unlist(check[1])[unlist(check[1])<1000], main = "mu of D2(>1000 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<100], main = "mu of D2(>100 omitted)")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of D2(>40 omitted)")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of D2(>40 omitted)", xlab = "mu")

jpeg(filename="parameters/D2_mu_Hist.jpg")
plot(hist(unlist(check[1])[unlist(check[1])<40]),main = "mu of D2(>40 omitted)", xlab = "mu")
dev.off()
jpeg(filename="parameters/D2_mu_BoxP.jpg")
boxplot(unlist(check[1])[unlist(check[1])<40], main = "mu of D2(>40 omitted)")
dev.off()

## theta
summary(unlist(check[2]))
boxplot(unlist(check[2]), main = "theta of D2")
boxplot(unlist(check[2])[unlist(check[2])<10^10], main = "theta of D2(>10^10 omitted)")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of D2(>10^1 omitted)")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of D2(>10^1 omitted)", xlab = "theta")

jpeg(filename="parameters/D2_theta_Hist.jpg")
plot(hist(unlist(check[2])[unlist(check[2])<10^1]),main = "theta of D2(>10^1 omitted)", xlab = "theta")
dev.off()
jpeg(filename="parameters/D2_theta_BoxP.jpg")
boxplot(unlist(check[2])[unlist(check[2])<10^1], main = "theta of D2(>10^1 omitted)")
dev.off()

## pi
summary(unlist(check[3]))
boxplot(unlist(check[3]), main = "pi of D2")
plot(hist(unlist(check[3])), main = "pi of D2", xlab = "pi")

jpeg(filename="parameters/D2_pi_Hist.jpg")
plot(hist(unlist(check[3])), main = "pi of D2", xlab = "pi")
dev.off()
jpeg(filename="parameters/D2_pi_BoxP.jpg")
boxplot(unlist(check[3]), main = "pi of D2")
dev.off()

##scatterplot for D2 group
check = result_D2

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[2])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of D2(>40 omitted)", ylab = "theta of D2(>10^1 omitted)")

plot(unlist(check[1])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "mu of D2(>40 omitted)", ylab = "pi of D2")

plot(unlist(check[2])[unlist(check[1])<40 & unlist(check[2])<10^1], 
     unlist(check[3])[unlist(check[1])<40 &unlist(check[2])<10^1],
     xlab = "theta of D2(>10^1 omitted)", ylab = "pi of D2")


par(mfrow=c(2,2))


### 3-D plots

unlist(result_H1)[unlist(result_H1[1])<40 & unlist(result_H1[2])<10^2] %>%
  matrix( nrow=3, byrow=T) %>%
  t() %>%
  data.frame() %>% 
  plot_ly( x = ~X1, y = ~X2, z = ~X3, type="scatter3d", mode = "markers") %>%
  plotly::layout(
    title = "3D plot of H1 group",
    scene = 
      list(
      xaxis = list(title = "mu"),
      yaxis = list(title = "theta", range = c(0,70)),
      zaxis = list(title = "pi")
    ))

unlist(result_H2)[unlist(result_H2[1])<40 & unlist(result_H2[2])<10^2] %>%
  matrix(nrow=3, byrow=T) %>%
  t() %>%
  data.frame() %>%
  plot_ly( x = ~X1, y = ~X2, z = ~X3, type="scatter3d", mode = "markers") %>%
  plotly::layout(
    title = "3D plot of H2 group",
    scene = 
      list(
        xaxis = list(title = "mu"),
        yaxis = list(title = "theta", range = c(0,70)),
        zaxis = list(title = "pi")
      ))

unlist(result_D1)[unlist(result_D1[1])<40 & unlist(result_D1[2])<10^2] %>%
  matrix(nrow=3, byrow=T) %>%
  t() %>%
  data.frame() %>%
  plot_ly( x = ~X1, y = ~X2, z = ~X3, type="scatter3d", mode = "markers") %>%
  plotly::layout(
    title = "3D plot of D1 group",
    scene = 
      list(
        xaxis = list(title = "mu"),
        yaxis = list(title = "theta", range = c(0,70)),
        zaxis = list(title = "pi")
      ))

unlist(result_D2)[unlist(result_D2[1])<40 & unlist(result_D2[2])<10^2] %>%
  matrix(nrow=3, byrow=T) %>%
  t() %>%
  data.frame() %>%
  plot_ly( x = ~X1, y = ~X2, z = ~X3, type="scatter3d", mode = "markers") %>%
  plotly::layout(
    title = "3D plot of D2 group",
    scene = 
      list(
        xaxis = list(title = "mu"),
        yaxis = list(title = "theta", range = c(0,70)),
        zaxis = list(title = "pi")
      ))
#############
#############
#############
unlist(result_H1)[unlist(result_H1[1])<40 & unlist(result_H1[2])<10^2] %>%
  matrix( nrow=3, byrow=T) %>%
  t() %>%
  data.frame() -> H1
H1$X4 = "H1"

unlist(result_H2)[unlist(result_H2[1])<40 & unlist(result_H2[2])<10^2] %>%
  matrix( nrow=3, byrow=T) %>%
  t() %>%
  data.frame() -> H2
H2$X4 = "H2"

unlist(result_D1)[unlist(result_D1[1])<40 & unlist(result_D1[2])<10^2] %>%
  matrix( nrow=3, byrow=T) %>%
  t() %>%
  data.frame() -> D1
D1$X4 = "D1"

unlist(result_D2)[unlist(result_D2[1])<40 & unlist(result_D2[2])<10^2] %>%
  matrix( nrow=3, byrow=T) %>%
  t() %>%
  data.frame() -> D2
D2$X4 = "D2"

data = rbind(H1, H2, D1, D2)

plot = rbind(H1, H2, D1, D2) %>% 
  plot_ly( x = ~X1, y = ~X2, z = ~X3,  color = ~X4, colors = "Set3",
           type="scatter3d", mode = "markers",
           marker = list(
             size = 3
                       )) %>%
  plotly::layout(
    title = "3D plot of parameters",
    scene = 
      list(
        xaxis = list(title = "mu"),
        yaxis = list(title = "theta", range = c(0,70)),
        zaxis = list(title = "pi")
      )) 


# aF0wWBiXoNXxXQqnGVqz is my API key
Sys.setenv("plotly_username"="BoyangT")
Sys.setenv("plotly_api_key"="aF0wWBiXoNXxXQqnGVqz")

api_create(plot, filename = "3D scatter plots for parameters of ZINB")
