P1=matrix(c(12,5 ,0,
            32,40,26,
            10,21,54),
          nrow = 3,
          dimnames = list("Badanie1" = c("Slaba", "Srednia", "Bardzo dobra"), 
                          "Badanie2" = c("Slaba", "Srednia", "Bardzo dobra")))
P1

data <- matrix(c(5, 2, 1, 0,0,0,
         6, 3, 2, 2,0,0,
         1, 4, 5, 5,2,2,
         0,10,15,18,5,2,
         1, 2, 5, 3,2,2,
         0, 1, 3, 4,3,2), 6, 6, byrow = T)
dimnames(data) <- list(
  Wyniki_z_kolokwium_2 = c("2", "3","+3","4","+4","5"),
  Wyniki_z_kolokwium_1 = c("2", "3","+3","4","+4","5")
  )
data
mcnemar.test(data, correct=TRUE)
mcnemar.test(data, correct=FALSE)

count <- c(5, 2, 1, 0,0,0,
           6, 3, 2, 2,0,0,
           1, 4, 5, 5,2,2,
           0,10,15,18,5,2,
           1, 2, 5, 3,2,2,
           0, 1, 3, 4,3,2)
Badanie1<-gl(6,6,labels=c("2", "3","+3","4","+4","5")) 
Badanie2<-gl(6,1,labels=c("2", "3","+3","4","+4","5")) 
DaneP1 <- data.frame(Badanie1,Badanie2,count)
DaneP1

library(gnm)
symmetry <- glm(count ~ Symm(Badanie1, Badanie2), data=DaneP1, family = poisson)
summary(symmetry)
x <- symmetry$deviance
x
r<-summary(symmetry)$"df"[2]
p <- 1 - pchisq(x, r)
p
####zad1b
quasi.symm <- glm(count ~ Badanie1 + Badanie2 + Symm(Badanie1,Badanie2),data=DaneP1, family =poisson)
summary(quasi.symm)
x <- quasi.symm$deviance
x
r <- summary(quasi.symm)$"df"[2]
r
p=1-pchisq(x,r)
p

##########zad1c
quasi.indep <- glm(count ~ Badanie1 + Badanie2 + Diag(Badanie1, Badanie2),data=DaneP1, family = poisson)
summary(quasi.indep)
x<-quasi.indep$deviance
x
r <- summary(quasi.indep)$"df"[2]
r
p<-1-pchisq(x,r)
p
#zad2???????????????
comparison <- anova(symmetry, quasi.symm)

p <- 1-pchisq(comparison$Deviance[2], comparison$Df[2])
p
