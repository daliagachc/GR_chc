wd=event_GR_long %>%
  dplyr::filter(score10>=5 & airChange==0 & R >=0.75)

mask=npfevent_size_frame[,c("posixTime","eventID")]

wd=merge(wd,mask,by="eventID") %>%
  dplyr::rename("localTime"=posixTime) %>%
  mutate(localDate=format(localTime,"%Y-%m-%d")) %>%
  select(size,GR,R,localDate)
wd=unique(wd)

wd2=acsm_data_long %>%
  dplyr::filter(hour(localTime) %in% seq(7,10)) %>%
  mutate(localDate=format(localTime, "%Y-%m-%d")) %>%
  dplyr::group_by(localDate,magnitude) %>%
  dplyr::summarise(value=median(value,na.rm=T))

wd3=merge(wd,wd2,by="localDate")

wd3$value=log10(wd3$value)
wd3$GR=log10(wd3$GR)

ggplot(data=subset(wd3,magnitude=="Chloride"))+
  geom_point(aes(x=value,y=GR,col=size))+
  geom_smooth(aes(x=value,y=GR,col=size),method=lm)
# 
# ggplot(data=wd3)+
#   geom_boxplot(aes(x=magnitude,y=GR,col=size))