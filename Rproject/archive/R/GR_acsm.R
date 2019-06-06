wd1=acsm_data_wide %>%
  mutate(YMD=format(DateUTC,"%Y-%m-%d")) %>%
  dplyr::filter(hour(DateUTC) %in% c(11,12,13,14,15,16))

#the GR data

wd2=event_GR_wide %>%
  mutate(YMD=format(first,"%Y-%m-%d"))
#wd2=subset(wd2,R_07_20 >= 0.75)
med=median(wd2$GR_03_07,na.rm=T)
wd2=wd2 %>%
  mutate(abs_med_der=abs(GR_03_07-med))
medmad=median(wd2$abs_med_der,na.rm=T)

wd2=wd2 %>%
  mutate(modz=0.6745*abs_med_der/medmad)
#wd2=subset(wd2,modz <=3.5)


mask=npfevent_size_frame%>%
  select(eventID,ion)
mask=unique(mask)

wd2=merge(wd2,mask) %>%
  dplyr::filter(ion=="negative")

wd3=CS_data %>%
  mutate(YMD=format(localTime,"%Y-%m-%d"))%>%
  rename("CS"=V2)%>%
  dplyr::filter(hour(localTime) %in% c(7,8))%>%
  select(CS,YMD)

wd4=merge(wd1,wd2,by="YMD")
wd4=merge(wd4,wd3,by="YMD")%>%
  dplyr::group_by(YMD) %>%
  dplyr::summarise(GR=median(GR_03_07),
                   CS=median(CS,na.rm=T),
                   sulf=median(Sulfate,na.rm=T),
                   nit=median(Nitrate,na.rm=T),
                   org=median(Organics,na.rm=T),
                   amm=median(Ammonium,na.rm=T),
                   chlor=median(Chloride,na.rm=T),
                   test=median(Nitrate)+median(Organics)+median(Sulfate)+median(Ammonium))



ggplot(data=wd4,aes(y=GR,x=CS))+
  geom_point()+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")