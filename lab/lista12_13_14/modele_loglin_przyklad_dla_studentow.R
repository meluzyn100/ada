### ANALIZOWANE DANE POCHODZ? Z KSI??KI Agresti (Tabela 6.3)

## PRZYGOTOWUJEMY DANE DO ANALIZY

# dane
seniors <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), 
                 dim = c(2,2,2), 
                 dimnames = list("cigarette" = c("yes","no"),
                                 "marijuana" = c("yes","no"),
                                 "alcohol" = c("yes","no")))
seniors

# dane - przedstawienie w tablicy
ftable(seniors, row.vars = c("alcohol","cigarette"))

# dodajmy liczno?ci brzegowe
addmargins(seniors)

# sp?jrzmy na proporcje
prop.table(seniors, margin = c(1,3))

# zmieniamy format danych
seniors.df <- as.data.frame(as.table(seniors))
seniors.df
seniors.df[,-4] <- lapply(seniors.df[,-4], relevel, ref = "no") #ustawiamy referencje na "no"
seniors.df

## MODELOWANIE, Model 1

mod1 <- glm(Freq ~ cigarette + marijuana + alcohol, 
            data = seniors.df, family = poisson)

summary(mod1)

# wyznaczamy p-warto??
1-pchisq(deviance(mod1), df = df.residual(mod1))


# por?wnujemy liczno?ci z modelem
cbind(mod1$data, fitted(mod1))

exp(4.17254+0.64931-0.31542+1.78511) #warto?? dla cigaretteyes, marijuanayes, alcoholyes
exp(4.17254+0-0.31542+1.78511) #warto?? dla cigaretteno, marijuanayes, alcoholyes itd.

## MODELOWANIE, Model 2

mod2 <- glm(Freq ~ cigarette + marijuana + alcohol + 
              cigarette*marijuana + marijuana*alcohol + cigarette*alcohol, 
            data = seniors.df, family = poisson)
summary(mod2)

# r?wnowa?nie
#mod2 <- glm(Freq ~ (cigarette + marijuana + alcohol)^2, 
#             data = seniors.df, family = poisson)
#lub
#mod2 <- glm(Freq ~ 
#              cigarette*marijuana + marijuana*alcohol + cigarette*alcohol, 
#            data = seniors.df, family = poisson)

#wyznaczamy p-warto??
1-pchisq(deviance(mod2), df = df.residual(mod2))

# por?wnujemy liczno?ci z modelem
cbind(mod2$data, fitted(mod2))

exp(5.63342-1.88667-5.30904+0.48772+2.84789+2.05453+2.98601) #warto?? dla cigaretteyes, marijuanayes, alcoholyes
exp(5.63342+0-5.30904+0.48772+0+0+2.98601) #warto?? dla cigaretteno, marijuanayes, alcoholyes itd.

#test <- anova(mod1,mod2)
#1-pchisq(test$Deviance[2], df = test$Df[2])