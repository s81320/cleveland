# to be sourced from dataEval-baseCase-rs-03.R

# required input / data : td
# visualises td


#### boxplots ####
##################

p<-ggplot(td, aes(x=as.factor(size), y=acc.ratio)) + 
  geom_boxplot() +
  labs(title = 'accuracy ratios for randomly sampled sub-forests')
print(p)

p<-ggplot(td, aes(x=as.factor(size), y=acc.sf)) + 
  geom_boxplot() +
  labs(title = 'accuracies for randomly sampled sub-forests')
print(p)

# as the number of trees in the sub-forest grows
# the mean accuracy ratio goes up and standard deviation goes down

#### 2d plots ####
##################


### mean oob accuracy explaining accuracy ratios ####
#####################################################

# why is this so bad ??
plot(acc.ratio~moa.sf 
     , data=td
     , asp=1 
     , xlab='mean individual oob accuracy'
     , ylab='ensemble accuracy ratio (on val set)'
     , main='explain forest performance\nby mean of tree performance')

library(ggplot2)
#install.packages('ggExtra')
library(ggExtra) # for the marginal plot
#install.packages('hrbrthemes')
library(hrbrthemes) # theme_ipsum

p1 <- ggplot(td, aes(x=moa.sf, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
p2 <- ggMarginal(p1, type="density")
print(p2)
rm(p1,p2)


#### mean dissimilarity explaining accuracy ratio ####
######################################################

p3 <- ggplot(td, aes(x=md.sf, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
p4 <- ggMarginal(p3, type="density")
print(p4)
rm(p3,p4)

