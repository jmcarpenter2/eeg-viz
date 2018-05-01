library(tidyverse)
library(feather)
library(huge)

binned.dat <- as.data.frame(feather('data/binned_data'))

subject.dat <- binned.dat %>% group_by_(.dots=c('bin', 'subject')) %>% 
               summarise_all(funs(mean)) %>% subset(select=-channel)

for (tp in 0:max(subject.dat$bin)){
  one.bin <- subject.dat[subject.dat$bin == tp, ] %>% subset(select=-c(bin, subject)) %>% 
    as.matrix() %>% t()
  
  #one.cov <- cov(one.bin)
  
  one.huge <- huge(one.bin)
  
  one.huge.refit <- huge.select(one.huge, criterion='stars')
  
  adj.mat <- as.data.frame(as.matrix(one.huge.refit$refit))
  
  write_csv(adj.mat, paste0('data/adj_mat_tp', tp, '.csv'))
}

