wd=NAIS_clasification_file

wd[is.na(wd)]=0
wd=wd %>%
  mutate(eventDay=ifelse(classI==1 | classII==1,
                         "event",ifelse(undefined==1,"undefined",
                                        ifelse(nodata==1,"noData",
                                               ifelse(nucleation==1,"nucleation-noEvent","noEvent")))))%>%
  mutate(date=localTime) %>%
  select(date,eventDay)

wd2=acsm_data_long %>%
  mutate(date=format(localTime, "%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4")) %>%
  select(localTime,magnitude,value,date)

wd3=HOM_data %>%
  mutate(date=format(localTime, "%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="HOM_API",
         value=HOM_FORM_APITOF) %>%
  select(localTime,magnitude,value,date)


wd4=SA_data %>%
  rename("localTime"=Time)%>%
  mutate(date=format(localTime,"%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="SA_API",
         value=SA)%>%
  select(localTime,magnitude,value,date)

wd5=CS_kappa_data %>%
  mutate(date=format(localTime,"%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="CS_kappa",
         value=V2)%>%
  select(localTime,magnitude,value,date)



wd6=rbind(wd2,wd3)
wd6=rbind(wd6,wd4)
wd6=rbind(wd6,wd5)
wd6=merge(wd6,wd,by="date")

normfacs=wd6 %>%
  dplyr::group_by(magnitude) %>%
  dplyr::summarise(normfac=sqrt(sum(value^2,na.rm=T)),
                   median_normfac=median(value/normfac,na.rm=T))

wd7=merge(wd6,normfacs,by="magnitude") %>%
  mutate(value_norm=(value/normfac)/median_normfac)

wd7$magnitude=as.factor(wd7$magnitude)


wd8=wd7 %>%
  mutate(hour=hour(localTime))%>%
  dplyr::group_by(hour,magnitude,eventDay)%>%
  dplyr::summarise(value=median(value,na.rm=T),
                   value_norm=median(value_norm,na.rm=T),
                   numb=n())%>%
  as.data.frame()

test=subset(wd8,eventDay != "noData" )
test=subset(test,not(magnitude %in% c("Nitrate","Organics","Chloride","Sulfate","CS_kappa")))

test=subset(test,eventDay=="event")

p=ggplot(data=test,aes(x=hour,y=value_norm,col=magnitude))+
  geom_line(size=1)+
  facet_wrap(~eventDay,nrow=2)
p