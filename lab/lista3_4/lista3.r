library(binom)
# a
conf <- binom.confint(50,200)
ls <- conf$"upper"- conf$"lower"
cbind(conf,ls)
cbind(conf,ls)[ls == max(ls),]
###b
conf <- binom.confint(0,200)
ls <- conf$"upper"- conf$"lower"
cbind(conf,ls)
cbind(conf,ls)[ls == max(ls),]
