wd=npfevent_size_frame %>%
  mutate(UTCtime=posixTime+4*3600,
         YMD=format(UTCtime, "%Y-%m-%d"),
         )
tz(wd$UTCtime)="UTC"

wd=subset(wd,YMD=="2018-05-22" & ion=="negative")

ggplot(data=wd,aes(x=UTCtime,y=dp,group=eventID,col=eventID))+
  geom_line()+
  scale_x_datetime(date_breaks="2 hours")+
  theme(axis.text.x=element_text(angle=90))