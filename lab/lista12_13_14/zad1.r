dane <- read.csv("Ankieta.csv",sep = ";")
sen <- dane[,1]
bieganie <- dane[,2]
pies <- dane[,3]
ankieta <- table(sen, bieganie, pies)
ankieta
ankieta.df <- as.data.frame(as.table(ankieta))
ankieta.df
# ankieta.df[,-4] <- lapply(ankieta.df[,-4], relevel, ref = "no") #ustawiamy referencje na "no"
# ankieta.df

mod1 <- glm(Freq ~ sen + bieganie + pies + sen*bieganie, 
            data = ankieta.df, family = poisson)
summary(mod1)

1-pchisq(deviance(mod1), df = df.residual(mod1))
  
cbind(mod1$data, fitted(mod1))

