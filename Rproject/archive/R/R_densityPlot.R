wd=event_GR_long
wd=subset(wd, R >= 0.8)

ggplot(data=wd,aes(x=GR))+
  facet_wrap(~size)+
  geom_histogram()+
  scale_x_continuous(limits=c(0,40),breaks=seq(0,40,2))



wd2=subset(wd,size=="total" & GR >=14)