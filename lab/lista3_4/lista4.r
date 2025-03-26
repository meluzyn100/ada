library(binom)
require(ggplot2)
library(reshape2)


symulation <-function(n = 10, dp= 0.2,MCs = 1000){
  MCs <- MCs
  n <- n
  ps <- seq(0.01, 0.99, dp)
  N <- length(ps)
  
  data <- matrix(0,N,6)
  
  for (k in 1:N) {
    p <- ps[k]
    
    wilson_ok <- 0
    axact_ok  <- 0
    asymp_ok  <- 0
    
    wilson_l <- rep(0,MCs)
    axact_l  <- rep(0,MCs)
    asymp_l  <- rep(0,MCs)
    
    for (i in 1:MCs){
      x <- rbinom(1, n, p)
      wilson <- binom.wilson(x, n)
      exact  <- binom.exact(x, n)
      asymp  <- binom.asymp(x, n)
      
      wilson_l[i] <- wilson$upper - wilson$lower
      axact_l[i] <- exact$upper - exact$lower
      asymp_l[i] <- asymp$upper - asymp$lower
      
      if (wilson["lower"]<p && p < wilson["upper"]) {
        wilson_ok <- 1 + wilson_ok
      }
      if (exact["lower"]<p && p < exact["upper"]) {
        axact_ok <- 1 + axact_ok
      }
      if (asymp["lower"]<p && p < asymp["upper"]) {
        asymp_ok <- 1 + asymp_ok
      }
    }
    
    data[k,]<- c(  wilson_ok/MCs,
              axact_ok/MCs,
              asymp_ok/MCs,
              
              mean(wilson_l),
              mean(axact_l) ,
              mean(asymp_l) )
    print(c(k,  wilson_ok/MCs,
              axact_ok/MCs,
              asymp_ok/MCs,
              
              mean(wilson_l),
              mean(axact_l) ,
              mean(asymp_l) ))
  
  }
  return(data)
}
data <- symulation(n = 100, dp = 0.01)
data
df1 <- data.frame(wilsonp = data[,1], axact = data[,2],asymp = data[,3],p =ps)
df2 <- data.frame(wilsonp = data[,4], axact = data[,5],asymp = data[,6],p =ps)

df_p <- melt(df1,id.vars="p")
df_l <- melt(df2,id.vars="p")
df1
# df_p<- read.csv("p_pokrycia_n_100.csv")
# df_l <- read.csv("dlugosci_n_100.csv")

ggplot(df_p,aes(p, value,col=variable))+
  geom_point()+ geom_line()

ggplot(df_l, aes(p, value,col=variable))+
  geom_point()+ geom_line()

# write.csv(df_p, "p_pokrycia_n_100.csv")
# write.csv(df_l, "dlugosci_n_100.csv")
write.csv(data, "data_n_100.csv",row.names = FALSE)
