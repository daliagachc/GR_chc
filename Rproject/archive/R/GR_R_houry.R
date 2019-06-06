minR=0.75

#monthly boxplot
wd1=event_GR_long

wd2=npfevent_size_frame %>%
  select(eventID,posixTime,ion) %>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(start.hour=hour(min(posixTime)),
                   ion=first(ion))

wd3=merge(wd1,wd2)

mask2=eventIDclas %>%
  filter(score10 >=5 & airChange ==0) %>%
  select(eventID)
wd3=merge(wd3,mask2,by="eventID")


wd3=subset(wd3, R >= minR)
wd3$hour_int=cut(wd3$start.hour, breaks=c(7,9,23),include.lowest=T)

wd3=subset(wd3,size %in% c("03_07","07_20","20_80"))
wd3$size=factor(wd3$size)
levels(wd3$size) <- c("3 - 7 nm", "7 - 20 nm", "20 - 70 nm")

ptitle=paste0("Growth rates by hour, R^2 >=",minR)

wd4=wd3 %>%
  dplyr::group_by(hour_int,size)%>%
  dplyr::summarise(numb=n())


ggplot(data=wd3, aes(x=hour_int,y=GR,col=ion))+
  facet_wrap(~size)+
  geom_boxplot(varwidth=T,outlier.shape=NA,notch=F)+
  geom_text(data=wd4, aes(x=hour_int,y=-2,label=numb,group=size),col="red")+
  ylim(c(-3,20))+
  labs(title=ptitle,
       y="GR [ng/h]",x="hour of event start")