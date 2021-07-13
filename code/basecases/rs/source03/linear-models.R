# to be sourced from dataEval-baseCase-rs-03.R

# required input / data : td
# builds linear models on td

#### linear models built on td ####
###################################

summary(lm(acc.ratio~. , data= td)) %>% print() # ??

# summary(lm(acc.ratio~. , data= td[,c(3,5:7)]))

lm1<-lm(acc.ratio~size+moa.sf+md.sf , data=td)
summary(lm1) %>% print()
# this is so poor :-(
# R sqrd almost 0 , mean oob acc and mean d1 dissim not significant
# and with negative estimates :-(
# plot(lm1)

# don't need this for now ...
predict(lm1,
        newdata=data.frame('size'=11 , 'moa.sf'=0.7 , 'md.sf'=0.3))
