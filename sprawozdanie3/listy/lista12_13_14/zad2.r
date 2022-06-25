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

co <- coef(mod1)
co


diff <- cbind(mod1$data, fitted(mod1))
diff
##########a
pa_mod1 <- sum(diff[diff["sen"] == 1 & diff["bieganie"] ==1,]["fitted(mod1)"])/sum(diff[diff["bieganie"] ==1,]["fitted(mod1)"])
pa_mod1
pb_mod1 <- sum(diff[diff["sen"] == 1 & diff["bieganie"] ==1,]["Freq"])/sum(diff[diff["bieganie"] ==1,]["Freq"])
pb_mod1


############b
pb_mod1 <- sum(diff[diff["bieganie"] ==1&diff["pies"] == 1 ,]["fitted(mod1)"])/sum(diff[diff["pies"] ==1,]["fitted(mod1)"])
pb_mod1
pb_mod1 <- sum(diff[diff["bieganie"] ==1&diff["pies"] == 1 ,]["Freq"])/sum(diff[diff["pies"] ==1,]["Freq"])
pb_mod1


############################model2


mod2 <- glm(Freq ~ sen + bieganie + pies + sen*bieganie + bieganie*pies, 
            data = ankieta.df, family = poisson)
summary(mod2)

1-pchisq(deviance(mod2), df = df.residual(mod2))

co2 <- coef(mod2)
co2


diff2 <- cbind(mod2$data, fitted(mod2))
diff2
##########a
pa_mod2 <- sum(diff2[diff2["sen"] == 1 & diff2["bieganie"] ==1,]["fitted(mod2)"])/sum(diff2[diff2["bieganie"] ==1,]["fitted(mod2)"])
pa_mod2
pa_mod2 <- sum(diff2[diff2["sen"] == 1 & diff2["bieganie"] ==1,]["Freq"])/sum(diff2[diff2["bieganie"] ==1,]["Freq"])
pa_mod2


############b
pb_mod2 <- sum(diff2[diff2["bieganie"] ==1&diff2["pies"] == 1 ,]["fitted(mod2)"])/sum(diff2[diff2["pies"] ==1,]["fitted(mod2)"])
pb_mod2
pb_mod2 <- sum(diff2[diff2["bieganie"] ==1&diff2["pies"] == 1 ,]["Freq"])/sum(diff2[diff2["pies"] ==1,]["Freq"])
pb_mod2


