library(stats)
?stats::binom.test

N <- 1
n <- 100
p <- 0.5
p1 <- 0.5
p2 <- 0.2
x <- rbinom(N, n, p1)
x
y <- rbinom(N, n, p2)
y
test <- binom.test(x, n, alternative = "t")
test

?stats::prop.test
test <- prop.test(x, n, alternative = "t", correct = T)
test
test <- prop.test(c(x,y),c(n,n), alternative = "t", correct = T)
test
##############
# zad2 greater x44,n200
# jak wygladają chipotezy
n <- 200
x <- 44
#a
binom.test(44, n, p=1/4, alternative = "g")
prop.test(44, n, p=1/4, alternative = "g", correct = T)
prop.test(44, n, p=1/4, alternative = "g", correct = F)
#b
binom.test(44, n,p = 0.5, alternative = "t")
prop.test(44, n, p = 0.5, alternative = "t", correct = T)
prop.test(44, n, p = 0.5, alternative = "t", correct = F)
#c
binom.test(50, n,p=1/5, alternative = "l")
prop.test(50, n, p=1/5, alternative = "l", correct = T)
prop.test(50, n, p=1/5, alternative = "l", correct = F)
##############d
#d.a
binom.test(22, 90, p=1/4, alternative = "g")
prop.test(22, 90, p=1/4, alternative = "g", correct = T)
prop.test(22, 90, p=1/4, alternative = "g", correct = F)
#d.b
binom.test(22, 90,p = 0.5, alternative = "t")
prop.test(22, 90, p = 0.5, alternative = "t", correct = T)
prop.test(22, 90, p = 0.5, alternative = "t", correct = F)
#d.c
binom.test(0, 90,p=1/5, alternative = "l")
prop.test(0, 90, p=1/5, alternative = "l", correct = T)
prop.test(0, 90, p=1/5, alternative = "l", correct = F)

