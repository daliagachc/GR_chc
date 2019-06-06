getDuration=function(time){
  
  begin = min(time)
  end=max(time)
  duration=as.numeric(end-begin)
  return(duration)
}

wd=npfevent_size_frame %>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(begin.hour=hour(min(posixTime)),
                   duration=getDuration(posixTime))

#beginning hour
ggplot(data=wd,aes(x=begin.hour))+
  #geom_density()+
  geom_bar()+
  labs(x="Hour of the day (local)")

#duration
ggplot(data=wd,aes(x=cut(duration,breaks=seq(0,24,1))))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90))