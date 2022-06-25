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

##############a
mod1 <- glm(Freq ~ sen + bieganie + pies, 
            data = ankieta.df, family = poisson)
mod2 <- glm(Freq ~ (sen + bieganie + pies)^2, 
            data = ankieta.df, family = poisson)
mod3 <- glm(Freq ~ (sen + bieganie + pies)^3, 
           data = ankieta.df, family = poisson)

test1 <- anova(mod1,mod2)
1-pchisq(test1$Deviance[2], df = test1$Df[2])
test2 <- anova(mod1,mod3)
1-pchisq(test2$Deviance[2], df = test2$Df[2])

##############b
mod1 <- glm(Freq ~ sen + bieganie + pies + sen*bieganie, 
            data = ankieta.df, family = poisson)
mod2 <- glm(Freq ~ (sen + bieganie + pies)^2, 
            data = ankieta.df, family = poisson)
mod3 <- glm(Freq ~ (sen + bieganie + pies)^3, 
            data = ankieta.df, family = poisson)

test1 <- anova(mod1,mod2)
1-pchisq(test1$Deviance[2], df = test1$Df[2])
test2 <- anova(mod1,mod3)
1-pchisq(test2$Deviance[2], df = test2$Df[2])

##############c
mod1 <- glm(Freq ~ sen + bieganie + pies + sen*bieganie + pies*bieganie, 
            data = ankieta.df, family = poisson)
mod2 <- glm(Freq ~ (sen + bieganie + pies)^2, 
            data = ankieta.df, family = poisson)#model niepełny
mod3 <- glm(Freq ~ (sen + bieganie + pies)^3, 
            data = ankieta.df, family = poisson)#model pełny

test1 <- anova(mod1,mod2)
1-pchisq(test1$Deviance[2], df = test1$Df[2])
test2 <- anova(mod1,mod3)
1-pchisq(test2$Deviance[2], df = test2$Df[2])mod1 <- glm(Freq ~ sen + bieganie + pies + sen*bieganie + pies*bieganie, 
                                                         data = ankieta.df, family = poisson)
mod2 <- glm(Freq ~ (sen + bieganie + pies)^2, 
            data = ankieta.df, family = poisson)#model niepełny
mod3 <- glm(Freq ~ (sen + bieganie + pies)^3, 
            data = ankieta.df, family = poisson)#model pełny

