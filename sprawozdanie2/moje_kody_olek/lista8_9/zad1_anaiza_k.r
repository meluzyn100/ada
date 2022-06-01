library(ca)
library(ggplot2)
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

# pi. <- rowSums(p)
# pi.
r <- rowSums(p)
r
# p.i <- colSums(p)
# p.i

c <- colSums(p)
c
R <- nrow(data)
C <- ncol(data)
R
C

P<- matrix(p,R,C)
P

D_r <- diag(r)
D_r
D_c <- diag(c)
D_c

# inv(D_r)%*%P
# P%*%inv(D_c)

A <- sqrt( inv(D_r) ) %*% ( P - r %*% t(c) ) %*% sqrt( inv(D_c) )
A
s <- svd(A)
U <- s$u
D <- diag(s$d)
V <- s$v

F <- sqrt( inv(D_r) ) %*% U %*% D
F
G <- sqrt( inv(D_c) ) %*% V %*% D
G

F_df <- data.frame(x = c(F[,1],G[,1]),
                   y = c(F[,2],G[,2]),
                   name = c(rownames(p),colnames(p)),
                   col = c(rep("red",R),rep("blue",C))
                   )
F_df

plot(F[,1:2],col = "black" ,pch =19)
par(new=TRUE)
plot(G[,1:2],,col = "black" ,pch =19)
par(new=TRUE)
plot(ca(data))

ggplot(F_df, aes(x, y, label = name ,colour = col) ) +  geom_point() +
  geom_text(vjust = -1)

plot(ca(data))
ca(data)$rowcoord
ca(data)$colcoord
ca(data)$N

F_df
getAnywhere(ca.data.frame)

