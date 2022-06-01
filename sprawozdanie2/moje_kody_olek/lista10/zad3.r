# zad3
library(ggplot2)
library(reshape2)
n <- 10
mcs <- 1000
alpha <- 0.05
ps_1 <- numeric(mcs)
ps_2 <- numeric(mcs)
p2 <- seq(0.01,0.99,0.02)
p2_0 <- 0.5
for (mc in 1:mcs) {
  X <- rbinom(n,1, p = 0.5)
  Y <- rbinom(n, 1, p = p2_0)
  data <- matrix(0, nrow = 2, ncol = 2)
  for (i in 1:n) {
    x<-X[i]
    y<-Y[i]
    if (x==0 & y==0) {
      data[1,1] <- data[1,1] + 1
    } else if (x==0 & y==1){
      data[1,2] <- data[1,2] + 1
    }
    else if (x==1 & y==0){
      data[2,1] <- data[2,1] + 1
    } else{
      data[2,2] <- data[2,2] + 1
    }
  }
  y12 <- data[1,2]
  y21 <- data[2,1]
  p_data <- data/n
  p11 <- p_data[1,1]
  p12 <- p_data[1,2]
  p21 <- p_data[2,1]
  p22 <- p_data[2,2]
  
  p_.1 <- (data[1,1]+data[2,1])/n
  p_1. <- (data[1,1]+data[1,2])/n
  D <- p_1. - p_.1
  sig <- sqrt( ( p_1.*(1-p_1.) + p_.1*(1-p_.1) - 2*(p11*p22-p12*p21) )/n  )
  z <- D/sig
  p_value1 <- 2*(1 - pnorm(abs(z)))
  z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
  p_value2 <- 2*(1 - pnorm(abs(z0)))
  ps_1[mc] <- p_value1 < 0.05
  ps_2[mc] <- p_value2 < 0.05
}
print(c(sum(ps_1, na.rm = TRUE)/mcs,
        sum(ps_2, na.rm = TRUE)/mcs))
#####
f_to_sup <- function(p) {
  p2 <- p
  for (mc in 1:mcs) {
    X <- rbinom(n,1, p = 0.5)
    Y <- rbinom(n, 1, p = p2)
    data <- matrix(0, nrow = 2, ncol = 2)
    for (i in 1:n) {
      x<-X[i]
      y<-Y[i]
      if (x==0 & y==0) {
        data[1,1] <- data[1,1] + 1
      } else if (x==0 & y==1){
        data[1,2] <- data[1,2] + 1
      }
      else if (x==1 & y==0){
        data[2,1] <- data[2,1] + 1
      } else{
        data[2,2] <- data[2,2] + 1
      }
    }
    y12 <- data[1,2]
    y21 <- data[2,1]
    p_data <- data/n
    p11 <- p_data[1,1]
    p12 <- p_data[1,2]
    p21 <- p_data[2,1]
    p22 <- p_data[2,2]
    
    p_.1 <- (data[1,1]+data[2,1])/n
    p_1. <- (data[1,1]+data[1,2])/n
    D <- p_1. - p_.1
    sig <- sqrt( ( p_1.*(1-p_1.) + p_.1*(1-p_.1) - 2*(p11*p22-p12*p21) )/n  )
    z <- D/sig
    p_value1 <- 2*(1 - pnorm(abs(z)))
    z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
    p_value2 <- 2*(1 - pnorm(abs(z0)))
    ps_1[mc] <- p_value1 < 0.05
    ps_2[mc] <- p_value2 < 0.05
  }
  return(c(sum(ps_1, na.rm = TRUE)/mcs,
           sum(ps_2, na.rm = TRUE)/mcs))
}


f_to_sup_error_rm <- function(x){
  tryCatch(
    expr = {
      # message(f_to_sup(x))
      # message("Successfully executed the log(x) call.")
      return(f_to_sup(x))
    },
    error = function(e){
      # message('Caught an error!')
      # print(e)
      # return(2)
    },
    warning = function(w){
      # print(x)
      f_to_sup(x)
      # return(3)
    },
    finally = {# plot(p2,sym[1,],type ="l")
      # lines(p2,sym[2,])
      # message('All done, quitting.')
    }
  )    
}

# f_to_sup_error_rm(0.11)
sym <- sapply(p2, f_to_sup_error_rm)
sym
df <- data.frame(p2 = p2, Z0 = sym[1,], Z = sym[2,] )
df


melt_Df <- melt(df, id=1, measure=c("Z","Z0"))
melt_Df
ggplot(melt_Df,aes(p2, value, colour = variable)) + 
geom_point() + geom_line() + ggtitle("Porównanie mocy testów")

