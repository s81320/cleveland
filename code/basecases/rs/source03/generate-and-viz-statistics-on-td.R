# to be sourced from dataEval-baseCase-rs-03.R

# required input / data : td
# builds statistics on td and visualises the statistics
# saves statistics to .rda (-> you can also load the previously saved statistics)

#### extract statistics from tidy table on randomly sampled sub-forests ####
############################################################################

stats<-td %>% 
  group_by(size) %>% 
  summarise('mean.acc'=mean(acc.sf)
            , 'sd.acc'=sd(acc.sf)
            , 'mean.acc.ratio'=mean(acc.ratio)
            , 'sd.acc.ratio'=sd(acc.ratio)
            , 'mean.mean.oob.acc'= mean(moa.sf)
            , 'sd.mean.oob.acc'=sd(moa.sf)
            , 'mean.mean.d1.dissim'=mean(md.sf)
            , 'sd.mean.d1.dissim'=sd(md.sf)
  ) %>% 
  data.frame()

stats

# why is the mean of the mean oob accuracy so high, 81% ? The full forest has much less, 77%

#### save generated statistics  ####
####################################

baseCase.rs.stats<- list(info=paste('created with source03/generate-an-viz.R on',Sys.time()
                                    , '\n what else should be in here?'
                                    , '\n list of content: stats')
                  , stats=stats)

file='data/10forests/baseCase-rs-stats.rda'
#save(baseCase.rs.stats , file=file)
#load(file)
#stats<-baseCase.rs.stats$stats

#### visualisations for the generated statistics ####
#####################################################

plot(mean.acc~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy)'
     , xlab='forest size'
     , ylab='mean accuracy'
     , type='b')
# numeric values just plotted
stats$mean.acc

plot(sd.acc~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy)'
     , xlab='forest size'
     , ylab='standard deviation for accuracy'
     , type='b')
# numeric values just plotted
stats$sd.acc

plot(mean.acc.ratio~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy ratios)'
     , xlab='frest size'
     , ylab='mean accuracy ratio'
     , type='b')
# numeric values just plotted
stats$mean.acc.ratio

plot(sd.acc.ratio~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy ratios)'
     , xlab='forest size'
     , ylab='standard deviation for accuracy ratio'
     , type='b')
# numeric values just plotted
stats$sd.acc.ratio
