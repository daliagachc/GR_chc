wd= npfevent_size_frame %>%
  mutate(day=format(posixTime, "%Y-%m-%d"))

severalEvents=wd %>%
  dplyr::group_by(day) %>%
  summarise(maxEv=max(dailyevent))

#test=merge(wd,subset(severalEvents,maxEv>=2),all=F)

test=subset(wd,day=="2018-05-30" & ion=="positive")

ggplot(data=test,aes(x=posixTime,y=dp,col=eventID))+
  geom_line()+
  theme(legend.position="none")