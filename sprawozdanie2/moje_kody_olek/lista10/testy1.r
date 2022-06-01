data <- matrix(c(32,44,
                 22,38), 2, 2, byrow = T)
data
dimnames(data) <- list(
  wyniki_z_1 = c("F", "T"),
  wyniki_z_2 = c("F", "T")
)
data
mcnemar.test(data,correct = FALSE)
 

y12 <- data[1,2]
y21 <- data[2,1]
z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
p <- 1 - pchisq(z0^2, df = 1)
p

zad1 <- function(data) {
  y12 <- data[1,2]
  y21 <- data[2,1]
  z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
  p <- 1 - pchisq(z0^2, df = 1)
  return(p)
}

############################
#zad2a
data2 <- matrix(c(1,5,
                 2,4), 2, 2, byrow = T)
data2
dimnames(data2) <- list(
  wyniki_z_1 = c("F", "T"),
  wyniki_z_2 = c("F", "T")
)
data2

zad2 <- function(data, correct = TRUE) {
  y12 <- data[1,2]
  y21 <- data[2,1]
  if (correct) {
    z0 <- (abs( y12 - y21 ) - 1)/sqrt(y12 + y21)
  } else {
    z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
  }
  p <- 1 - pchisq(z0^2, df = 1)
  return(p)
}
mcnemar.test(data2,correct = TRUE)
zad2(data2)

#zad2b
library(exact2x2)
data2
mcnemar.exact(data2)$p.value
y12 <- data2[1,2]
y21 <- data2[2,1]
2*sum(sapply(0:y12, function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))
2*sum(sapply(y12:(y12+y21), function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))

if (y12 < (y12+y21)/2) {
  2*sum(sapply(0:y12, function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))
} else if (y12 > (y12+y21)/2) {
  2*sum(sapply(y12:(y12+y21), function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))
} else {
  1
}

zad2b <- function(data) {
  y12 <- data[1,2]
  y21 <- data[2,1]
  if (y12 < (y12+y21)/2) {
    p <- 2*sum(sapply(0:y12, function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))
  } else if (y12 > (y12+y21)/2) {
    p <- 2*sum(sapply(y12:(y12+y21), function(k) choose(y12+y21,k) * (1/2)^k * (1/2)^(y12+y21-k)))
  } else {
    p <- 1
  }
  return(p)
}

zad2b(data2)
mcnemar.exact(data2)$p.value

#################################zad3
n <-100
sym <- rmultinom(1, size = n, prob = c(0.25,0.25,0.25,0.25))
sym
# sym <- sample(1:100,4)
data <- matrix(sym,2,2)
data

############################3
n <-10
mcs <- 2
alpha <- 0.05
p2 <- 0.5
X <- rbinom(n,1, p = 0.5)
X
Y <- rbinom(n, 1, p = p2)
Y
data <- matrix(0, nrow = 2, ncol = 2)
dimnames(data) <- list(
  X = c("F", "T"),
  Y = c("F", "T")
)
data
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
p_value1
z0  <- ( y12 - y21 ) / ( sqrt( y12 + y21 ) )  
p_value2 <- 2*(1 - pnorm(abs(z0)))
p_value2

####################

ps_value1 <- numeric(mc)
ps_value2 <- numeric(mc)
