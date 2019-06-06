wd=HgMetClusterBcCOCO2CH4Hourly[,c("Time","co2")] %>%
  mutate(YMD=format(Time, "%Y-%m-%d")) %>%
  dplyr::group_by(YMD) %>%
  dplyr::summarise(co2median=median(co2,na.rm=T))

wd2=event_GR_long

wd3=npfevent_size_frame %>%
  select(posixTime,eventID) %>%
  mutate(YMD=format(posixTime, "%Y-%m-%d")) %>%
  select(YMD,eventID)
wd3=unique(wd3)
  
wd4=merge(wd2,wd3,by="eventID")
wd5=merge(wd4,wd,by="YMD")

wd5=subset(wd5, R >= 0.8)
select=subset(wd5,size=="20_60")