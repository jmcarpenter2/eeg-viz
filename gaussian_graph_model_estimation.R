library(tidyverse)
library(feather)
library(huge)

binned.dat <- as.data.frame(feather('data/binned_data_notsmoothed'))

subject.dat <- binned.dat %>% group_by_(.dots=c('bin', 'subject')) %>% 
               summarise_all(funs(mean)) %>% subset(select=-channel)
channel.dat <- binned.dat %>% group_by_(.dots=c('bin', 'channel')) %>% 
  summarise_all(funs(mean)) %>% subset(select=-subject)

for (tp in 0:max(subject.dat$bin)){
  print(paste0('Timepoint: ', tp))
  
  one.bin <- subject.dat[subject.dat$bin == tp, ] %>% subset(select=-c(bin, subject)) %>% 
    as.matrix() %>% t()
  
  #one.cov <- cov(one.bin)
  
  one.huge <- huge(one.bin)
  
  one.huge.refit <- huge.select(one.huge, criterion='stars')
  
  adj.mat <- as.data.frame(as.matrix(one.huge.refit$refit))
  
  write_csv(adj.mat, paste0('data/subj_adjmat_tp', tp, '.csv'))
}

store_adjmats <- function(binned.df, prefix){
    channel.dat <- binned.df %>% group_by_(.dots=c('bin', 'channel')) %>% 
    summarise_all(funs(mean)) %>% subset(select=-subject)
    for (tp in 0:max(channel.dat$bin)){
      print(paste0('Timepoint: ', tp))
      
      one.bin <- channel.dat[channel.dat$bin == tp, ] %>% subset(select=-c(bin, channel)) %>% 
        as.matrix() %>% t()
      
      #one.cov <- cov(one.bin)
      
      one.huge <- huge(one.bin)
      
      one.huge.refit <- huge.select(one.huge, criterion='stars')
      
      adj.mat <- as.data.frame(as.matrix(one.huge.refit$refit))
      
      write_csv(adj.mat, paste0('data/',prefix,'chan_adjmat_tp', tp, '.csv'))
    }
}



binned.dat.alc <- as.data.frame(feather('data/alc_binned_data'))
binned.dat.ctrl <- as.data.frame(feather('data/ctrl_binned_data'))

store_adjmats(binned.dat.alc, 'alc')
store_adjmats(binned.dat.ctrl, 'ctrl')
