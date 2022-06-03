# library(dsc)


m1a <- matrix(c(888,369,50,457,263,95,10,99,208,29,2,44,78,9,0,19,1,0,0,4),nrow=5,ncol = 4,byrow = T)
dimnames(m1a) <- list(c("18-25","26-35","36-45","46-59","60+"),c("A","B","C","D"))
m1a

m2a <- matrix(c(505,202,19,136,240,77,14,88,181,63,8,105,512,159,21,294),nrow=4,byrow = T)
dimnames(m2a) <- list(c("Wie´s","Miasto do 20 tys.", "Miasto 20-50 tys.","Miasto pow. 50 tys. "),c("A","B","C","D"))
m2a


m1b<-matrix(c(888,369,50,457,263,95,10,99,208,29,2,44,79,9,0,23),nrow=4,byrow=T)
dimnames(m1b) <- list(c("18-25","26-35","36-45","46+"),c("A","B","C","D"))
m1b


m1c<-matrix(c(888,369,50,457,263,95,10,99,287,38,2,67),nrow=3,byrow=TRUE)
dimnames(m1c) <- list(c("18-25","26-35","36+"),c("A","B","C","D"))
m1c

data <- m1a
data

p <- data/sum(data)
p

pi. <- rowSums(p)
pi.

p.i <- colSums(p)
p.i
R <- nrow(data)
C <- ncol(data)
R
C

d1 <- 0
for (i in 1:R) {
  for (j in 1:C) {
    d1 <- p[i, j]^2/pi.[i] + d1
  }
}
d2 <- sum(p.i^2)
d2
d1

tau <- ( d1 - d2 )/(1 - d2)
tau

tau <- function(data) {
  p <- data/sum(data)
  pi. <- rowSums(p)
  p.i <- colSums(p)
  R <- nrow(data)
  C <- ncol(data)
  
  d1 <- 0
  for (i in 1:R) {
    for (j in 1:C) {
      d1 <- p[i, j]^2/pi.[i] + d1
    }
  }
  d2 <- sum(p.i^2)
  return( ( d1 - d2 )/(1 - d2) )
}
tau(m1a)[[1]]