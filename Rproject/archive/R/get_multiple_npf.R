wd=npfevent_size_frame %>%
  filter(ion=="positive")%>%
  select(posixTime,eventID) %>%
  mutate(YMD=format(posixTime, "%Y-%m-%d")) %>%
  select(YMD,eventID)
wd=unique(wd)

repDays=wd %>%
  dplyr::group_by(YMD) %>%
  dplyr::summarise(count=n(),
                   IDs=paste(eventID,collapse="-")) %>%
  mutate(posixTime=as.POSIXct(YMD))

ggplot(data=repDays,aes(x=posixTime,y=count))+
  geom_point()+
  scale_x_datetime(date_breaks="1 month")
