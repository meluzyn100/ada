library(MASS)
sen <- dane[,1]
bieganie <- dane[,2]
pies <- dane[,3]
ankieta <- table(sen, bieganie, pies)
ankieta
ankieta.df <- as.data.frame(as.table(ankieta))
ankieta.df
#123
mod123 <- glm(Freq ~ sen + bieganie + pies + 
                  sen*bieganie + 
                  sen*pies +
                  bieganie*pies+
                  sen*bieganie*pies, 
            data = ankieta.df, family = poisson)
1-pchisq(deviance(mod123), df = df.residual(mod123))

#12 13 23
mod12_13_23 <- glm(Freq ~ sen + bieganie + pies + 
              sen*bieganie + 
              sen*pies +
              bieganie*pies, 
            data = ankieta.df, family = poisson)
1-pchisq(deviance(mod12_13_23), df = df.residual(mod12_13_23))

#12 13
mod12_13 <- glm(Freq ~ sen + bieganie + pies + 
            sen*bieganie + 
            sen*pies, 
            data = ankieta.df, family = poisson)
1-pchisq(deviance(mod12_13), df = df.residual(mod12_13))

#12 23
mod12_23 <- glm(Freq ~ sen + bieganie + pies + 
              sen*bieganie +
              bieganie*pies, 
            data = ankieta.df, family = poisson)

#13 23
mod13_23 <- glm(Freq ~ sen + bieganie + pies + 
              sen*pies +
              bieganie*pies, 
            data = ankieta.df, family = poisson)
1-pchisq(deviance(mod13_23), df = df.residual(mod13_23))

#12 3
mod12_3 <- glm(Freq ~ sen + bieganie + pies + 
                  sen*bieganie,
                data = ankieta.df, family = poisson)

#13 2
mod13_2 <- glm(Freq ~ sen + bieganie + pies + 
                 sen*pies,
               data = ankieta.df, family = poisson)

#23 1
mod23_1 <- glm(Freq ~ sen + bieganie + pies + 
                 bieganie*pies,
               data = ankieta.df, family = poisson)
#12
mod12 <- glm(Freq ~ sen + bieganie + 
                 sen*bieganie,
               data = ankieta.df, family = poisson)
#13
mod13 <- glm(Freq ~ sen + pies + 
               sen*pies,
             data = ankieta.df, family = poisson)
#23
mod23 <- glm(Freq ~  bieganie + pies +  
               bieganie*pies,
             data = ankieta.df, family = poisson)
#1 2 3
mod1_2_3 <- glm(Freq ~ sen + bieganie + pies,
             data = ankieta.df, family = poisson)

#1 2
mod1_2 <- glm(Freq ~ sen + bieganie ,
                data = ankieta.df, family = poisson)

#1 3
mod1_3 <- glm(Freq ~ sen + pies,
                data = ankieta.df, family = poisson)

#2 3
mod2_3 <- glm(Freq ~ bieganie + pies,
                data = ankieta.df, family = poisson)
#1
mod1 <- glm(Freq ~ sen,
                data = ankieta.df, family = poisson)

#2
mod2 <- glm(Freq ~ bieganie,
                data = ankieta.df, family = poisson)
#3
mod3 <- glm(Freq ~ pies,
                data = ankieta.df, family = poisson)

#staly
mod0 <- glm(Freq ~ 1,
                data = ankieta.df, family = poisson)


test1 <- anova(mod1_2_3, mod13_2)
1-pchisq(test1$Deviance[2], df = test1$Df[2]) # wybieramy mod1_2_3

test2 <- anova(mod1_2_3, mod12_3)
1-pchisq(test2$Deviance[2], df = test2$Df[2]) # wybieramy mod12_3

test3 <- anova(mod12_3, mod12_13)
1-pchisq(test3$Deviance[2], df = test3$Df[2]) # wybieramy mod12_3

test4 <- anova(mod13_2, mod13_23)
1-pchisq(test4$Deviance[2], df = test4$Df[2]) # wybieramy mod12_3

