wd=acsm_data_long %>%
  mutate(localTime=DateUTC)
attributes(wd$localTime)$tzone="etc/GMT+4"


acsm_data_long_5m=wd %>%
  mutate(minute=minute(localTime)) %>%
  mutate(min_group=((minute %/%5)*5)) %>%
  dplyr::group_by(YMDH= paste(year(localTime),month(localTime),
                              day(localTime),hour(localTime),min_group,
                              sep="-"),magnitude) %>%
  dplyr::summarise(value=median(value,na.rm=T),
                   numb=n())%>%
  as.data.frame() %>%
  mutate(localTime=as.POSIXct(YMDH,format="%Y-%m-%d-%H-%M",tz="etc/GMT+4"))%>%
  mutate(YMDH=NULL)

