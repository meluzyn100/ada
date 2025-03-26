sample(1:10,5)
for (i in 1:10) {
  print(sample(1:10,5,replace = TRUE))#bez zwracania
}


for (i in 1:10) {
  print(sample(1:10,5,replace = TRUE))#ze zwracaniem
}

for (i in 1:10) {
  print(sample(c("yes","no"),5,replace = TRUE))#ze zwracaniem
}

for (i in 1:10) {
  print(sample(c("yes","no"),5,replace = TRUE, prob = c(0.1,0.9))) #ze zwracaniem
}
#zad1 
#bez wracania
View(mtcars)
rows <- row.names(mtcars)
names <- sample(rows, floor(length(rows)/10) )
mtcars[names ,] #losowanie po nazwach

ind <- sample(x=nrow(mtcars),
              size = 0.1*nrow(mtcars))#losowanie po indeksach
mtcars[ind,]

#ze zwracaniem
view(mtcars)
rows <- row.names(mtcars)
names <- sample(rows, floor(length(rows)/10),replace = TRUE)
mtcars[names ,] #losowanie po nazwach

ind <- sample(x=nrow(mtcars),
              size = 0.1*nrow(mtcars),
              replace = TRUE)#losowanie po indeksach
mtcars[ind,]


#zad2

n<-10
p<-0.1

Y <- sample(x = c(1,0), size = n, replace = TRUE, prob = c(p, 1-p))
X <- sum(Y)
rep(0,10)



#z zajec
binom.rv <- function(n,p,N){
  X <- rep(0,N)
  for (i in 1:N) {
    r = sum(runif(n) < p)
    # r = sum(sample(x = c(1,0), size = n, replace = TRUE, prob = c(p, 1-p)))
    X[i] = r
  }
  return(X)
} 
n<-10
p<-0.5
N <- 10000
data <- binom.rv(n, p ,N)
hist(data,breaks = max(data))


#moja
binom.rv2 <- function(n,p,N){
  X <- rep(0,N)
  for (i in 1:N) {
    r = sum(sample(x = c(1,0), size = n, replace = TRUE, prob = c(p, 1-p)))
    X[i] = r
  }
  return(X)
} 

n<-10
p<-0.5
N <- 10000
data1 <- binom.rv(n, p ,N)
hist(data1,breaks = max(data1))
hist(rbinom(N,n,p), breaks = max(data), add = T,col = "red")

data2 <- binom.rv2(n, p ,N)
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
hist(data1,breaks = max(data2),col = "black")
hist(rbinom(N,n,p), breaks = max(data), add = T,col = c1)

legend("topleft", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"))

#zad3

multinom.rv <-function(n, p, N){
  k <- length(p)
  X <- matrix(0, nrow = k, ncol = N)
  for (j in 1:N) {
    ind <- sample(1:k, n, replace = TRUE, prob = p)
    for (i in 1:n) {
      X[ind[i],j] = 1 + X[ind[i],j]
    }
  }
  rownames(X) <- sprintf("p%d", 1:k)
  return(X)
}

n <- 10
p <- c(0.2,0.3,0.5)
N <- 1

k <- length(p)
X <- matrix(0, nrow = k, ncol = N)

for (j in 1:N) {
  ind <- sample(1:k, n, replace = TRUE, prob = p)
  print(ind)
  for (i in 1:n) {
    X[ind[i],j] = 1 + X[ind[i],j]
  }
}
X
rownames(X) <- sprintf("p%d", 1:k)
return(X)





X <- multinom.rv(n,p,N)
X
rowMeans(X) #warości oczekiwane z pruby EX_i
p*n #Wartosci ozcekiwane zmiennej losowej

apply(X,1, var)
n*p*(1-p)
